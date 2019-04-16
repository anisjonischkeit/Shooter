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
};

type state = {
  player: playerState,
  keys,
};

let boardWidth = 600;
let playerSquareWidth = 30;
let playerDiameter = playerSquareWidth * 2;
let playerX =
  float_of_int(boardWidth) /. 2.0 -. float_of_int(playerSquareWidth);
let playerY =
  float_of_int(boardWidth) /. 2.0 -. float_of_int(playerSquareWidth);

let setup = env => {
  Env.size(~width=boardWidth, ~height=boardWidth, env);

  {
    player: {
      rotation: 0.0,
      bullets: [],
      position: (playerX, playerY),
      lastShotFrame: 0,
    },
    keys: {
      left: Released,
      right: Released,
      space: Released,
    },
  };
};

let drawPlayer = (rotation, env) => {
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
  Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
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
  Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=255), env);
  Draw.rect(~pos=((-1), (-1)), ~width=2, ~height=2, env);

  Draw.popMatrix(env);

  // rotation centre for player
  Draw.fill(Utils.color(~r=255, ~g=255, ~b=0, ~a=255), env);
  Draw.rect(~pos=((-1), (-1)), ~width=2, ~height=2, env);

  Draw.popMatrix(env);
};

let getNextRotation = (rotation, keys) => {
  let speed = 2.5;
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
let getBulletOffsets = (rotation: float, travelDistance: float) => {
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

let getBulletStartPos = rotation => {
  let (offsetX, offsetY) =
    getBulletOffsets(rotation, playerDiameter / 2 |> float_of_int);

  // TODO: these numbers were pretty much guessed and I feel like
  // there's some dodgyness here that I'll be paying for later
  (265.0 +. 20.0 +. offsetX, 255.0 +. 23.0 +. offsetY);
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
  let shotInterval = 20; // can shoot once every n frames
  let canShoot = frameCount - player.lastShotFrame >= shotInterval;

  switch (keys.space, canShoot) {
  | (Pressed, true) =>
    let startPos = getBulletStartPos(player.rotation);
    let bulletSpeed = 3.0;
    let (bulletOffsetX, bulletOffsetY) =
      getBulletOffsets(player.rotation, bulletSpeed);
    [
      {
        position: startPos,
        nextPosition: (_startPos, (currX, currY)) => {
          (currX +. bulletOffsetX, currY +. bulletOffsetY);
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

let drawRotation = (rotation, env) => {
  Draw.text(~body=rotation |> string_of_float, ~pos=(0, 0), env);
};

let drawBullets = (bullets, env) => {
  Draw.fill(Utils.color(~r=120, ~g=255, ~b=255, ~a=255), env);

  // TODO[PERF]: this could probably get composed with the other map if we
  // get performance issues with too many bullets

  bullets
  |> List.iter((bullet: bullet) =>
       Draw.ellipsef(~center=bullet.position, ~radx=2.0, ~rady=2.0, env)
     );
};

let draw = (previousState: state, env) => {
  let nextKeys = getNextKeys(previousState.keys, env);
  let nextRotation = getNextRotation(previousState.player.rotation, nextKeys);
  let nextBulletPositions =
    getNextBulletPositions(previousState.player.bullets);
  let newBullets =
    getNewBullets(previousState.player, nextKeys, Env.frameCount(env));
  let nextBullets = newBullets |> List.append(nextBulletPositions);

  Draw.background(Utils.color(~r=255, ~g=217, ~b=229, ~a=255), env);

  drawPlayer(Utils.radians(nextRotation), env);
  drawRotation(nextRotation, env);
  drawBullets(nextBullets, env);

  {
    player: {
      rotation: nextRotation,
      bullets: nextBullets,
      position: previousState.player.position,
      lastShotFrame:
        newBullets |> List.length > 0
          ? Env.frameCount(env) : previousState.player.lastShotFrame,
    },
    keys: nextKeys,
  };
};

run(~setup, ~draw, ());