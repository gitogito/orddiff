let euler ~dt ~t ~x ~xdot =
  x +. (xdot ~t ~x) *. dt

let rk4 ~dt ~t ~x ~xdot =
  let k1 = xdot ~t ~x in
  let k2 = xdot ~t:(t +. dt /. 2.0) ~x:(x +. dt /. 2.0 *. k1) in
  let k3 = xdot ~t:(t +. dt /. 2.0) ~x:(x +. dt /. 2.0 *. k2) in
  let k4 = xdot ~t:(t +. dt) ~x:(x +. dt *. k3)  in
  x +. dt /. 6.0 *. (k1 +. 2.0 *. k2 +. 2.0 *. k3 +. k4)
