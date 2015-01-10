open Printf

let dxdt _t x f =
  f.(0) <- x.(1);
  f.(1) <- -.x.(0)

let xs_euler = [| 1.0; 0.0 |]
let xs_rk4 = [| 1.0; 0.0 |]

let loop n ode_euler ode_rk4 =
  let rec aux i =
    if i > n then
      ()
    else begin
      let t = ode_euler.Orddiff.Base.get_t () in
      printf "%g\t%g\t%g\n" t xs_euler.(0) xs_rk4.(0);
      ode_euler.Orddiff.Base.update ();
      ode_rk4.Orddiff.Base.update ();
      aux (i + 1)
    end
  in
  aux 0

let () =
  let n = 100 in
  let dt = 0.1 in
  let ode_euler = Orddiff.Euler.init ~dt ~dxdt ~xs:xs_euler in
  let ode_rk4 = Orddiff.Rk4.init ~dt ~dxdt ~xs:xs_rk4 in
  loop n ode_euler ode_rk4
