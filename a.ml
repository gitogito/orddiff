open Printf
open ExtLib

let xdot ~t ~x ~y =
  y

let ydot ~t ~x ~y =
  -.x

let xdot_ary = [|
  (fun ~t ~x_ary -> x_ary.(1));
  (fun ~t ~x_ary -> -.x_ary.(0));
|]

let rec loop i n dt x_ary_euler x_ary_rk4 =
  if i > n then
    ()
  else begin
    let t = float i *. dt in
    let x_ary_euler = Orddiff.euler_x ~dt ~t ~x_ary:x_ary_euler ~xdot_ary in
    let x_ary_rk4 = Orddiff.rk4_x ~dt ~t ~x_ary:x_ary_rk4 ~xdot_ary in
    printf "%g\t%g\t%g\n" t x_ary_euler.(0) x_ary_rk4.(0);
    loop (i + 1) n dt x_ary_euler x_ary_rk4
  end

let () =
  let n = 100000 in
  let dt = 0.01 in
  let x_ary_euler = [| 1.0; 0.0 |] in
  let x_ary_rk4 = [| 1.0; 0.0 |] in
  loop 1 n dt x_ary_euler x_ary_rk4
