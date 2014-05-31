module Base = struct
  type t = {
    update : unit -> unit;
    get_x_ary : unit -> float array;
  }
end

module Euler = struct
  include Base

  type t' = {
    dt : float;
    mutable t : float;
    mutable x_ary : float array;
    xdot_ary : (t:float -> x_ary:float array -> float) array;
  }

  let update self () =
    self.t <- self.t +. self.dt;
    self.x_ary <- Array.mapi
                    (fun i x -> x +. (self.xdot_ary.(i) ~t:self.t ~x_ary:self.x_ary) *. self.dt)
                    self.x_ary

  let get_x_ary (self : t') () =
    self.x_ary

  let init ~dt ~x_ary ~xdot_ary =
    let self = {
      dt;
      t = 0.0;
      x_ary;
      xdot_ary;
    } in
    { update = update self;
      get_x_ary = get_x_ary self; }
end

module Rk4 = struct
  include Base

  type t' = {
    dt : float;
    mutable t : float;
    mutable x_ary : float array;
    xdot_ary : (t:float -> x_ary:float array -> float) array;
  }

  let update self () =
    self.t <- self.t +. self.dt;

    let order = Array.length self.x_ary in
    let f1_ary = Array.make order 0.0 in
    let f2_ary = Array.make order 0.0 in
    let f3_ary = Array.make order 0.0 in
    let f4_ary = Array.make order 0.0 in
    let dt2 = self.dt /. 2.0 in

    for i = 0 to order - 1 do
      f1_ary.(i) <- self.xdot_ary.(i) ~t:self.t ~x_ary:self.x_ary
    done;

    let x_ary' = Array.mapi (fun i x -> x +. dt2 *. f1_ary.(i)) self.x_ary in
    for i = 0 to order - 1 do
      f2_ary.(i) <- self.xdot_ary.(i) ~t:(self.t +. dt2) ~x_ary:x_ary'
    done;

    let x_ary' = Array.mapi (fun i x -> x +. dt2 *. f2_ary.(i)) self.x_ary in
    for i = 0 to order - 1 do
      f3_ary.(i) <- self.xdot_ary.(i) ~t:(self.t +. dt2) ~x_ary:x_ary'
    done;

    let x_ary' = Array.mapi (fun i x -> x +. self.dt *. f3_ary.(i)) self.x_ary in
    for i = 0 to order - 1 do
      f4_ary.(i) <- self.xdot_ary.(i) ~t:(self.t +. self.dt) ~x_ary:x_ary'
    done;

    for i = 0 to order - 1 do
      let f1 = f1_ary.(i) in
      let f2 = f2_ary.(i) in
      let f3 = f3_ary.(i) in
      let f4 = f4_ary.(i) in
      self.x_ary.(i) <- self.x_ary.(i) +. self.dt /. 6.0 *. (f1 +. 2.0 *. f2 +. 2.0 *. f3 +. f4)
    done

  let get_x_ary (self : t') () =
    self.x_ary

  let init ~dt ~x_ary ~xdot_ary =
    let self = {
      dt;
      t = 0.0;
      x_ary;
      xdot_ary;
    } in
    { update = update self;
      get_x_ary = get_x_ary self; }
end
