open Printf

let xdot ~t ~x ~y =
  y

let ydot ~t ~x ~y =
  -.y

let rec loop i n dt x_euler y_euler x_rk4 y_rk4 =
  if i > n then
    ()
  else begin
    let t = float i *. dt in
    let x_euler', y_euler' = Orddiff.euler_2 ~dt ~t ~x:x_euler ~y:y_euler ~xdot ~ydot in
    let x_rk4', y_rk4' = Orddiff.rk4_2 ~dt ~t ~x:x_rk4 ~y:y_rk4 ~xdot ~ydot in
    printf "%g\t%g\t%g\n" t x_euler' x_rk4';
    loop (i + 1) n dt x_euler' y_euler' x_rk4' y_rk4'
  end

let () =
  let n = 100000 in
  let dt = 0.0001 in
  let x0 = 1.0 in
  let y0 = -1.0 in
  loop 1 n dt x0 y0 x0 y0
