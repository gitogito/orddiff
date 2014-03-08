open Printf

let xdot ~t ~x =
  x

let rec loop i n dt x_euler x_rk4 =
  if i > n then
    ()
  else begin
    let t = float i *. dt in
    let x_euler' = Orddiff.euler ~dt ~t ~x:x_euler ~xdot in
    let x_rk4'   = Orddiff.rk4   ~dt ~t ~x:x_rk4   ~xdot in
    printf "%g\t%g\t%g\n" t x_euler' x_rk4';
    loop (i + 1) n dt x_euler' x_rk4'
  end

let () =
  let n = 100 in
  loop 1 n (1.0 /. float n) 1.0 1.0
