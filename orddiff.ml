let euler ~dt ~t ~x ~xdot =
  x +. (xdot ~t ~x) *. dt

let rk4 ~dt ~t ~x ~xdot =
  let dt2 = dt /. 2.0 in
  let f1 = xdot ~t ~x in
  let f2 = xdot ~t:(t +. dt2) ~x:(x +. dt2 *. f1) in
  let f3 = xdot ~t:(t +. dt2) ~x:(x +. dt2 *. f2) in
  let f4 = xdot ~t:(t +. dt) ~x:(x +. dt *. f3) in
  x +. dt /. 6.0 *. (f1 +. 2.0 *. f2 +. 2.0 *. f3 +. f4)

let euler_2 ~dt ~t ~x ~y ~xdot ~ydot =
  let x' = x +. (xdot ~t ~x ~y) *. dt in
  let y' = y +. (ydot ~t ~x ~y) *. dt in
  x', y'

let rk4_2 ~dt ~t ~x ~y ~xdot ~ydot =
  let dt2 = dt /. 2.0 in
  let f1 = xdot ~t ~x ~y in
  let g1 = ydot ~t ~x ~y in
  let f2 = xdot ~t:(t +. dt2) ~x:(x +. dt2 *. f1) ~y:(y +. dt2 *. g1) in
  let g2 = ydot ~t:(t +. dt2) ~x:(x +. dt2 *. f1) ~y:(y +. dt2 *. g1) in
  let f3 = xdot ~t:(t +. dt2) ~x:(x +. dt2 *. f2) ~y:(y +. dt2 *. g2) in
  let g3 = ydot ~t:(t +. dt2) ~x:(x +. dt2 *. f2) ~y:(y +. dt2 *. g2) in
  let f4 = xdot ~t:(t +. dt) ~x:(x +. dt *. f3) ~y:(y +. dt *. g3) in
  let g4 = ydot ~t:(t +. dt) ~x:(x +. dt *. f3) ~y:(y +. dt *. g3) in
  let x' = x +. dt /. 6.0 *. (f1 +. 2.0 *. f2 +. 2.0 *. f3 +. f4) in
  let y' = y +. dt /. 6.0 *. (g1 +. 2.0 *. g2 +. 2.0 *. g3 +. g4) in
  x', y'
