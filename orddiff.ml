module Base = struct
  type t = {
    update : unit -> unit;
    get_t : unit -> float;
  }
end

module Euler = struct
  include Base

  type t' = {
    dt : float;
    dxdt : float -> float array -> float array -> unit;
    xs : float array;
    mutable n : int;
    order : int;
    fs : float array;
  }

  let update self () =
    let t = self.dt *. float self.n in
    self.dxdt t self.xs self.fs;
    for i = 0 to self.order - 1 do
      self.xs.(i) <- self.xs.(i) +. self.fs.(i) *. self.dt
    done;
    self.n <- self.n + 1

  let get_t self () =
    self.dt *. float self.n

  let init ~dt ~dxdt ~xs =
    let order = Array.length xs in
    let self = {
      dt;
      dxdt;
      xs;
      n = 0;
      order;
      fs = Array.make order 0.0;
    } in
    {
      update = update self;
      get_t = get_t self;
    }
end

module Rk4 = struct
  include Base

  type t' = {
    dt : float;
    dxdt : float -> float array -> float array -> unit;
    xs : float array;
    mutable n : int;
    order : int;
    f1s : float array;
    f2s : float array;
    f3s : float array;
    f4s : float array;
    dt2 : float;
  }

  let update self () =
    let t = self.dt *. float self.n in
    self.dxdt t self.xs self.f1s;
    let xs' = Array.mapi (fun i x -> x +. self.dt2 *. self.f1s.(i)) self.xs in
    self.dxdt (t +. self.dt2) xs' self.f2s;
    let xs' = Array.mapi (fun i x -> x +. self.dt2 *. self.f2s.(i)) self.xs in
    self.dxdt (t +. self.dt2) xs' self.f3s;
    let xs' = Array.mapi (fun i x -> x +. self.dt *. self.f3s.(i)) self.xs in
    self.dxdt (t +. self.dt) xs' self.f4s;

    for i = 0 to self.order - 1 do
      let f1 = self.f1s.(i) in
      let f2 = self.f2s.(i) in
      let f3 = self.f3s.(i) in
      let f4 = self.f4s.(i) in
      self.xs.(i) <- self.xs.(i) +. self.dt /. 6.0 *. (f1 +. 2.0 *. f2 +. 2.0 *. f3 +. f4)
    done;
    self.n <- self.n + 1

  let get_t self () =
    self.dt *. float self.n

  let init ~dt ~dxdt ~xs =
    let order = Array.length xs in
    let self = {
      dt;
      dxdt;
      xs;
      n = 0;
      order;
      f1s = Array.make order 0.0;
      f2s = Array.make order 0.0;
      f3s = Array.make order 0.0;
      f4s = Array.make order 0.0;
      dt2 = dt /. 2.0;
    } in
    {
      update = update self;
      get_t = get_t self;
    }
end
