open Reprocessing;

type state = {rotation: float};

let boardWidth = 600;

let setup = env => {
  Env.size(~width=boardWidth, ~height=boardWidth, env);

  {rotation: 0.0};
};

let drawPlayer = (rotation, env) => {
  let width = 30;
  let x = boardWidth / 2 - width;
  let y = boardWidth / 2 - width;

  let xf = float_of_int(x);
  let yf = float_of_int(y);
  let widthf = float_of_int(width);
  let centreX = xf +. widthf /. 2.0;

  /*
   tipWidth
   c^2 = a^2 + b^2
   c^2 = a^2 + a^2 // square has the same width
   c^2 = 2(a^2)
   c^2 / 2 = a^2
   sqrt(c^2 / 2) = a
   */
  let tipWidth = sqrt(width * width / 2 |> float_of_int) |> int_of_float;

  // rotate the whole player
  let playerRotationCentreX = xf +. widthf /. 2.0;
  let playerRotationCentreY = yf +. widthf /. 4.0;
  Draw.pushMatrix(env);

  Draw.translate(~x=playerRotationCentreX, ~y=playerRotationCentreY, env);

  Draw.rotate(rotation, env);

  // create player
  Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
  Draw.rect(~pos=(- width / 2, - width / 4), ~width, ~height=width, env);

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

  ();
};

let draw = (state, env) => {
  Draw.background(Utils.color(~r=255, ~g=217, ~b=229, ~a=255), env);

  Draw.pushMatrix(env);

  drawPlayer(Utils.radians(state.rotation), env);

  Draw.popMatrix(env);

  Draw.text(
    ~body=mod_float(state.rotation, 360.0) |> string_of_float,
    ~pos=(0, 0),
    env,
  );
  // print_endline(mod_float(state.rotation, 360.0) |> string_of_float);
  state;
  {rotation: state.rotation +. 1.0};
};

run(~setup, ~draw, ());