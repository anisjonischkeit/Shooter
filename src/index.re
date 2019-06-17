open Reprocessing;

type keyState =
  | Pressed
  | Released;

type keys = {
  left: keyState,
  right: keyState,
  space: keyState,
};

type point = (float, float);
type startPos = point;

type bullet = {
  position: point,
  nextPosition: (startPos, point) => point,
  startPos,
};

type bullets = list(bullet);

type playerState = {
  rotation: float,
  position: point,
  bullets,
  lastShotFrame: int,
  color: (int, int, int),
};

type state = {
  player: playerState,
  keys,
};

module GQLData = [%graphql
  {|
   subscription {
     player_states {
       id
     }
   }
 |}
];

type decodePayload = {data: GQLData.t};

type decodedData = {
  type_: string,
  id: string,
  payload: decodePayload,
};

let wsQueue: Queue.t(decodedData) = Queue.create();

let gqlQuery = GQLData.make();
let gqlQueryStr = gqlQuery##query;

let decodeRes = str => {
  open Json.Decode;
  let decoder = json => {
    type_: json |> field("type", string),
    id: json |> field("id", string),
    payload:
      json
      |> field("payload", p => {data: p |> field("data", gqlQuery##parse)}),
  };

  try (Some(Json.parseOrRaise(str) |> decoder)) {
  | DecodeError(_e) => None
  };
};

let setupWebsocket = () => {
  open BsWebSocket;

  let ws =
    make("ws://localhost:8080/v1alpha1/graphql", ~protocol="graphql-ws", ());

  onOpen(
    ws,
    e => {
      onClose(
        ws,
        e => {
          Js.log2("Close", e);
          Js.log2("code", CloseEvent.code(e));
          Js.log2("reason", CloseEvent.reason(e));
          Js.log2("wasClean", CloseEvent.wasClean(e));
        },
      );
      onError(ws, Js.log2("error"));

      Js.log2("Open", e);
      send(
        ws,
        {|{"type":"connection_init","payload":{"headers":{"content-type":"application/json","x-hasura-admin-secret":"myadminsecretkey"}}}|},
      );
      send(
        ws,
        {j|{"id":"1","type":"start","payload":{"variables":{},"extensions":{},"operationName":null,"query": "$gqlQueryStr"}}|j},
      );
    },
  );
  onMessage(ws, e =>
    switch (MessageEvent.data(e) |> decodeRes) {
    | Some(data) => Queue.add(data, wsQueue)
    | None => ()
    }
  );

  Js.log2("url:", url(ws));
  Js.log2("rs:", readyState(ws) == Connecting);
};

let reconcileState = state => {
  /* not yet implemented */
  Js.log(wsQueue |> Queue.length);
  state;
};

let drawState = ({player: {rotation, position: (posX, posY)}}, env) => {
  let playerStr =
    "position: ("
    ++ (posX |> string_of_float)
    ++ ", "
    ++ (posY |> string_of_float)
    ++ ")";
  Draw.text(~body=playerStr, ~pos=(0, 30), env);
  Draw.text(
    ~body="rotation: " ++ (rotation |> string_of_float),
    ~pos=(0, 0),
    env,
  );
};

let boardWidth = 600;
let playerSquareWidth = 30;
let playerDiameter = playerSquareWidth * 2;
let playerX =
  float_of_int(boardWidth) /. 2.0 -. float_of_int(playerSquareWidth);
let playerY =
  float_of_int(boardWidth) /. 2.0 -. float_of_int(playerSquareWidth);

let setup = env => {
  setupWebsocket();

  Env.size(~width=boardWidth, ~height=boardWidth, env);

  {
    player: {
      rotation: 0.0,
      bullets: [],
      position: (playerX, playerY),
      lastShotFrame: 0,
      color: (0, 0, 0),
    },
    keys: {
      left: Released,
      right: Released,
      space: Released,
    },
  };
};

let drawPlayer = (rotation, (r, g, b), env) => {
  let screenX = boardWidth / 2 - playerSquareWidth;
  let screenY = boardWidth / 2 - playerSquareWidth;

  let screenXf = float_of_int(screenX);
  let screenYf = float_of_int(screenY);
  let widthf = float_of_int(playerSquareWidth);

  /*
   tipWidth
   c^2 = a^2 + b^2
   c^2 = a^2 + a^2 // square has the same width
   c^2 = 2(a^2)
   c^2 / 2 = a^2
   sqrt(c^2 / 2) = a
   */
  let tipWidth =
    sqrt(playerSquareWidth * playerSquareWidth / 2 |> float_of_int)
    |> int_of_float;

  // rotate the whole player
  let playerRotationCentreX = screenXf +. widthf /. 2.0;
  let playerRotationCentreY = screenYf +. widthf /. 4.0;
  Draw.pushMatrix(env);

  Draw.translate(~x=playerRotationCentreX, ~y=playerRotationCentreY, env);

  Draw.rotate(rotation, env);

  // create player
  Draw.fill(Utils.color(~r, ~g, ~b, ~a=255), env);
  Draw.rect(
    ~pos=(- playerSquareWidth / 2, - playerSquareWidth / 4),
    ~width=playerSquareWidth,
    ~height=playerSquareWidth,
    env,
  );

  Draw.pushMatrix(env);
  Draw.translate(~x=0.0, ~y=-. widthf /. 4.0, env);
  Draw.rotate(Utils.radians(45.0), env);

  Draw.rect(
    ~pos=(- tipWidth / 2, - tipWidth / 2),
    ~width=tipWidth,
    ~height=tipWidth,
    env,
  );

  // rotation centre for tip
  Draw.fill(Utils.color(~r=125, ~g=125, ~b=125, ~a=255), env);
  Draw.rect(~pos=((-1), (-1)), ~width=2, ~height=2, env);

  Draw.popMatrix(env);

  // rotation centre for player
  Draw.fill(Utils.color(~r=255, ~g=255, ~b=0, ~a=255), env);
  Draw.rect(~pos=((-1), (-1)), ~width=2, ~height=2, env);

  Draw.popMatrix(env);
};

let getNextRotation = (rotation, keys) => {
  let speed = 2.5 *. 4.;
  switch (keys.left, keys.right) {
  | (Pressed, Released) => mod_float(rotation -. speed, 360.0)
  | (Released, Pressed) => mod_float(rotation +. speed, 360.0)
  | _ => rotation
  };
};

/*
 h = CONSTANT
 q1x = sin(thet) = o/h
 q1x = sin(thet) * h = a
 q1x = sin(thet) * h

 q1y = cos(thet) = a/h
 q1y = cos(thet) * h = a
 q1y = cos(thet) * h

 thet=90 - (rotation-270)     |--             |   /                thet=rotation
 q4x = -cos, q4y = -sin    Q4 |   ----t-      |t/               Q1 q1x = sin, q1y = -cos
                                 |---------------+---------------|
 thet=90 - (rotation-180)  Q3               /t|         -t----| Q2 thet=rotation-90
 q3x = -sin, q3y = cos                    /   |             --|    q2x = cos, q2y = sin

 NOTE: where sin and cos should be used is based on the above image.
 where - and + should be used is based on the coord system having (0,0)
 as the top left and (∞, ∞) as the bottom right
 */
let getOffsetsForRotation = (rotation: float, travelDistance: float) => {
  let h = travelDistance; // hypotenuse

  if (rotation -. 270.0 >= 0.0) {
    // q4
    let theta = Utils.radians(rotation -. 270.0);
    let x = -. cos(theta) *. h;
    let y = -. sin(theta) *. h;
    // print_endline("q4 " ++ string_of_float(theta));
    (x, y);
  } else if (rotation -. 180.0 >= 0.0) {
    // q3

    let theta = Utils.radians(rotation -. 180.0);
    let x = -. sin(theta) *. h;
    let y = cos(theta) *. h;
    // print_endline("q3 " ++ string_of_float(theta));
    (x, y);
  } else if (rotation -. 90.0 >= 0.0) {
    // q2

    let theta = Utils.radians(rotation -. 90.0);
    let x = cos(theta) *. h;
    let y = sin(theta) *. h;
    // print_endline("q2 " ++ string_of_float(theta));
    (x, y);
  } else {
    // q1

    let theta = Utils.radians(rotation);
    let x = sin(theta) *. h;
    let y = -. cos(theta) *. h;
    // print_endline("q1 " ++ string_of_float(theta));
    (x, y);
  };
};

let getBulletRelativeStartPos = rotation => {
  let (offsetX, offsetY) =
    getOffsetsForRotation(rotation, playerDiameter / 2 |> float_of_int);

  // TODO: these numbers were pretty much guessed and I feel like
  // there's some dodgyness here that I'll be paying for later
  (265.0 +. 20.0 +. offsetX, 255.0 +. 23.0 +. offsetY);
};

let relToAbsPos = ((playerX, playerY): point, (relX, relY): point): point => {
  let halfBoardWidth = boardWidth / 2 |> float_of_int;
  (playerX -. halfBoardWidth +. relX, playerY -. halfBoardWidth +. relY);
};

let absToRelPos = ((playerX, playerY): point, (absX, absY): point): point => {
  /*
       The inverse of relToAbsPos (let's do some maths):

       absX = playerX -. halfBoardWidth +. relX
       absX -. playerX +. halfBoardWidth = relX
   */

  let halfBoardWidth = boardWidth / 2 |> float_of_int;
  (absX -. playerX +. halfBoardWidth, absY -. playerY +. halfBoardWidth);
};

let getNextBulletPositions = (bullets): bullets => {
  // continue moving the bullets in the right direction
  bullets
  |> List.map((bullet: bullet) =>
       {
         ...bullet,
         position: bullet.nextPosition(bullet.startPos, bullet.position),
       }
     );
};

let getNewBullets = (player, keys, frameCount) => {
  let shotInterval = 8; // can shoot once every n frames
  let canShoot = frameCount - player.lastShotFrame >= shotInterval;

  switch (keys.space, canShoot) {
  | (Pressed, true) =>
    let startPos =
      getBulletRelativeStartPos(player.rotation)
      |> relToAbsPos(player.position);
    let bulletSpeed = 3.0;
    let (bulletOffsetX, bulletOffsetY) =
      getOffsetsForRotation(player.rotation, bulletSpeed);
    [
      {
        position: startPos,
        nextPosition: (_startPos, (currBulletX, currBulletY)) => {
          (currBulletX +. bulletOffsetX, currBulletY +. bulletOffsetY);
        },
        startPos,
      },
    ];
  | _ => []
  };
};

let getKeyState = (key, currentState, env) =>
  if (Env.keyPressed(key, env)) {
    Pressed;
  } else if (Env.keyReleased(key, env)) {
    Released;
  } else {
    currentState;
  };

let getNextKeys = (keys, env) => {
  {
    left: getKeyState(Left, keys.left, env),
    right: getKeyState(Right, keys.right, env),
    space: getKeyState(Space, keys.space, env),
  };
};

let getNextPosition = (rotation, (positionX, positionY), movementSpeed) => {
  let (offsetX, offsetY) = getOffsetsForRotation(rotation, movementSpeed);

  (positionX +. offsetX, positionY +. offsetY);
};

let drawBullets = (bullets, playerPos, (r, g, b), env) => {
  Draw.fill(Utils.color(~r, ~g, ~b, ~a=255), env);

  // TODO[PERF]: this could probably get composed with the other map if we
  // get performance issues with too many bullets

  bullets
  |> List.iter((bullet: bullet) =>
       Draw.ellipsef(
         ~center=absToRelPos(playerPos, bullet.position),
         ~radx=2.0,
         ~rady=2.0,
         env,
       )
     );
};

let draw = (previousState: state, env) => {
  let reconciledState = reconcileState(previousState);

  let color = reconciledState.player.color;
  let nextKeys = getNextKeys(reconciledState.keys, env);
  let nextRotation =
    getNextRotation(reconciledState.player.rotation, nextKeys);
  let nextPosition =
    getNextPosition(
      reconciledState.player.rotation,
      reconciledState.player.position,
      0.1 *. 10.,
    ); //lag this one frame behind (ie right is clicked, player turns imediately but only starts moving in the direction next frame)

  let nextBulletPositions =
    getNextBulletPositions(reconciledState.player.bullets);
  let newBullets =
    getNewBullets(reconciledState.player, nextKeys, Env.frameCount(env));
  let nextBullets = newBullets |> List.append(nextBulletPositions);

  Draw.background(Utils.color(~r=255, ~g=217, ~b=229, ~a=255), env);

  drawPlayer(Utils.radians(nextRotation), color, env);
  drawBullets(nextBullets, nextPosition, color, env);

  drawState(reconciledState, env);

  {
    player: {
      color: reconciledState.player.color,
      rotation: nextRotation,
      bullets: nextBullets,
      position: nextPosition,
      lastShotFrame:
        newBullets |> List.length > 0
          ? Env.frameCount(env) : reconciledState.player.lastShotFrame,
    },
    keys: nextKeys,
  };
};

run(~setup, ~draw, ());