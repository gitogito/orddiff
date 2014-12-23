open Printf

let xdot ~t ~x =
  t /. x

let rec loop i n dt x_euler x_rk4 =
  if i > n then
    ()
  else begin
    let x_euler', x_rk4' =
      if i = 0 then
        (x_euler, x_rk4)
      else begin
        let t = float (i - 1) *. dt in
        (Orddiff.euler ~dt ~t ~x:x_euler ~xdot, Orddiff.rk4 ~dt ~t ~x:x_rk4 ~xdot)
      end
    in
    let t = float i *. dt in
    printf "%g\t%g\t%g\n" t x_euler' x_rk4';
    loop (i + 1) n dt x_euler' x_rk4'
  end

let () =
  let n = 10 in
  loop 0 n (1.0 /. float n) 1.0 1.0
