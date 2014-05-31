open Printf
open ExtLib

let xdot_ary = [|
  (fun ~t ~x_ary -> ignore t; x_ary.(1));
  (fun ~t ~x_ary -> ignore t; -.x_ary.(0));
|]

let rec loop i n dt euler rk4 =
  if i > n then
    (euler.Orddiff.Euler.get_x_ary (), rk4.Orddiff.Rk4.get_x_ary ())
  else begin
    let t = float i *. dt in
    euler.Orddiff.Euler.update ();
    rk4.Orddiff.Rk4.update ();
    printf "%g\t%g\t%g\n"
      t
      (euler.Orddiff.Euler.get_x_ary ()).(0)
      (rk4.Orddiff.Rk4.get_x_ary ()).(0);
    loop (i + 1) n dt euler rk4
  end

let () =
  let n = 1000 in
  let dt = 0.01 in
  let x_ary_euler = [| 1.0; 0.0 |] in
  let euler = Orddiff.Euler.init ~dt ~x_ary:x_ary_euler ~xdot_ary in
  let x_ary_rk4 = [| 1.0; 0.0 |] in
  let rk4 = Orddiff.Rk4.init ~dt ~x_ary:x_ary_rk4 ~xdot_ary in
  let x_ary_euler, x_ary_rk4 = loop 1 n dt euler rk4 in
  printf "#%g\t%g\n" x_ary_euler.(0) x_ary_rk4.(0)
