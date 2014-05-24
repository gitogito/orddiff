open Printf

let xdot y ~t ~x =
  y

let ydot x ~t ~x:y =
  ~-.x

let rec loop i n dt x_euler y_euler x_rk4 y_rk4 =
  if i > n then
    ()
  else begin
    let t = float i *. dt in
    let x_euler' = Orddiff.euler ~dt ~t ~x:x_euler ~xdot:(xdot y_euler) in
    let y_euler' = Orddiff.euler ~dt ~t ~x:y_euler ~xdot:(ydot x_euler) in
    let x_rk4' = Orddiff.rk4 ~dt ~t ~x:x_rk4 ~xdot:(xdot y_rk4) in
    let y_rk4' = Orddiff.rk4 ~dt ~t ~x:y_rk4 ~xdot:(ydot x_rk4) in
    printf "%g\t%g\t%g\n" t x_euler' x_rk4';
    loop (i + 1) n dt x_euler' y_euler' x_rk4' y_rk4'
  end

let () =
  let n = 100 in
  let dt = 10.0 /. float n in
  let x0 = 1.0 in
  let y0 = 0.0 in
  loop 1 n dt x0 y0 x0 y0
