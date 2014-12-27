open Printf
open ExtLib

let xdot_ary = [|
  (fun ~t ~x_ary -> ignore t; x_ary.(1));
  (fun ~t ~x_ary -> ignore t; -.x_ary.(0));
|]

let loop n dt euler rk4 =
  let rec aux i =
    if i > n then
      ()
    else begin
      printf "%g\t%g\t%g\n"
        (float i *. dt)
        (euler.Orddiff.Euler.get_x_ary ()).(0)
        (rk4.Orddiff.Rk4.get_x_ary ()).(0);
      euler.Orddiff.Euler.update ();
      rk4.Orddiff.Rk4.update ();
      aux (i + 1)
    end
  in
  aux 0

let () =
  let n = 100 in
  let dt = 0.1 in
  let x_ary_euler = [| 1.0; 0.0 |] in
  let euler = Orddiff.Euler.init ~dt ~x_ary:x_ary_euler ~xdot_ary in
  let x_ary_rk4 = [| 1.0; 0.0 |] in
  let rk4 = Orddiff.Rk4.init ~dt ~x_ary:x_ary_rk4 ~xdot_ary in
  loop n dt euler rk4
