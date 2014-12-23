open Printf
open ExtLib

let xdot_ary = [|
  (fun ~t ~x_ary -> ignore t; x_ary.(1));
  (fun ~t ~x_ary -> ignore t; -.x_ary.(0));
|]

let loop n dt euler rk4 =
  let print t x_euler x_rk4 =
    printf "%g\t%g\t%g\n" t x_euler x_rk4
  in
  let rec aux i n dt euler rk4 =
    print
      (float i *. dt)
      (euler.Orddiff.Euler.get_x_ary ()).(0)
      (rk4.Orddiff.Rk4.get_x_ary ()).(0);
    if i = n then
      ()
    else begin
      euler.Orddiff.Euler.update ();
      rk4.Orddiff.Rk4.update ();
      aux (i + 1) n dt euler rk4
    end
  in
  aux 0 n dt euler rk4

let () =
  let n = 100 in
  let dt = 0.1 in
  let x_ary_euler = [| 1.0; 0.0 |] in
  let euler = Orddiff.Euler.init ~dt ~x_ary:x_ary_euler ~xdot_ary in
  let x_ary_rk4 = [| 1.0; 0.0 |] in
  let rk4 = Orddiff.Rk4.init ~dt ~x_ary:x_ary_rk4 ~xdot_ary in
  loop n dt euler rk4
