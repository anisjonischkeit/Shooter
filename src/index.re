open Reprocessing;

type keyState =
  | Pressed
  | Released;

type keys = {
  left: keyState,
  right: keyState,
  space: keyState,
};

type point = (int, int);
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
};

type state = {
  player: playerState,
  keys,
};

let boardWidth = 600;
let playerWidth = 30;
let playerX = boardWidth / 2 - playerWidth;
let playerY = boardWidth / 2 - playerWidth;

let setup = env => {
  Env.size(~width=boardWidth, ~height=boardWidth, env);

  {
    player: {
      rotation: 0.0,
      bullets: [],
      position: (playerX, playerY),
    },
    keys: {
      left: Released,
      right: Released,
      space: Released,
    },
  };
};

let drawPlayer = (rotation, env) => {
  let screenX = boardWidth / 2 - playerWidth;
  let screenY = boardWidth / 2 - playerWidth;

  let screenXf = float_of_int(screenX);
  let screenYf = float_of_int(screenY);
  let widthf = float_of_int(playerWidth);

  /*
   tipWidth
   c^2 = a^2 + b^2
   c^2 = a^2 + a^2 // square has the same width
   c^2 = 2(a^2)
   c^2 / 2 = a^2
   sqrt(c^2 / 2) = a
   */
  let tipWidth =
    sqrt(playerWidth * playerWidth / 2 |> float_of_int) |> int_of_float;

  // rotate the whole player
  let playerRotationCentreX = screenXf +. widthf /. 2.0;
  let playerRotationCentreY = screenYf +. widthf /. 4.0;
  Draw.pushMatrix(env);

  Draw.translate(~x=playerRotationCentreX, ~y=playerRotationCentreY, env);

  Draw.rotate(rotation, env);

  // create player
  Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
  Draw.rect(
    ~pos=(- playerWidth / 2, - playerWidth / 4),
    ~width=playerWidth,
    ~height=playerWidth,
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
  Draw.rect(~pos=(0, 0), ~width=1, ~height=1, env);

  Draw.popMatrix(env);

  // rotation centre for player
  Draw.fill(Utils.color(~r=255, ~g=255, ~b=0, ~a=255), env);
  Draw.rect(~pos=(0, 0), ~width=1, ~height=1, env);

  Draw.popMatrix(env);
};

let getNextRotation = (rotation, keys) => {
  let speed = 2.5;
  switch (keys.left, keys.right) {
  | (Pressed, Released) => rotation -. speed
  | (Released, Pressed) => rotation +. speed
  | _ => rotation
  };
};

let getNextBullets = (player, keys): bullets => {
  let nextBullets =
    // continue moving the bullets in the right direction
    player.bullets
    |> List.map((bullet: bullet) =>
         {
           ...bullet,
           position: bullet.nextPosition(bullet.startPos, bullet.position),
         }
       );

  switch (keys.space) {
  | Pressed => [
      {
        position: player.position,
        nextPosition: (_startPos, (currX, currY)) => (currX + 1, currY + 1),
        startPos: player.position,
      },
      ...nextBullets,
    ]
  | Released => nextBullets
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
  Draw.text(
    ~body=mod_float(rotation, 360.0) |> string_of_float,
    ~pos=(0, 0),
    env,
  );
};

let drawBullets = (bullets, env) => {
  Draw.fill(Utils.color(~r=120, ~g=255, ~b=255, ~a=255), env);

  // TODO[PERF]: this could probably get composed with the other map if we
  // get performance issues with too many bullets
  bullets
  |> List.iter((bullet: bullet) =>
       Draw.ellipse(~center=bullet.position, ~radx=2, ~rady=2, env)
     );
};

let draw = (previousState: state, env) => {
  let nextKeys = getNextKeys(previousState.keys, env);
  let nextRotation = getNextRotation(previousState.player.rotation, nextKeys);
  let nextBullets = getNextBullets(previousState.player, nextKeys);

  Draw.background(Utils.color(~r=255, ~g=217, ~b=229, ~a=255), env);

  drawPlayer(Utils.radians(nextRotation), env);
  drawRotation(nextRotation, env);
  drawBullets(nextBullets, env);

  {
    player: {
      rotation: nextRotation,
      bullets: nextBullets,
      position: previousState.player.position,
    },
    keys: nextKeys,
  };
};

run(~setup, ~draw, ());