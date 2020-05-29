module type Monoid = {
  type t;

  let sum: (t, t) => t;
  let zero: t;
};

module IntMonoid = {
  type t = int;

  let sum = (n1, n2) => n1 + n2;
  let zero = 0;
};

module TuppleMonoid = {
  module Make = (Fst: Monoid, Snd: Monoid) => {
    type fst = Fst.t;
    type snd = Snd.t;
    type t = (fst, snd);

    let sum = ((f1, s1): t, (f2, s2): t) => (
      Fst.sum(f1, f2),
      Snd.sum(s1, s2),
    );
    let zero = (Fst.zero, Snd.zero);
  };
};

module Folder = {
  module Make = (M: Monoid) => {
    let fold = (ls: list(M.t)) => List.fold_left(M.sum, M.zero, ls);
  };
};

module IntTupleMonoid = TuppleMonoid.Make(IntMonoid, IntMonoid);
module IntFolder = Folder.Make(IntMonoid);

let summer =
    (type a, module SummerMod: Monoid with type t = a, values: list(a)) =>
  List.fold_left(SummerMod.sum, SummerMod.zero, values);

Js.log(summer((module IntMonoid), [1, 2, 3]));
Js.log(IntFolder.fold([1, 2, 3]));
Js.log(summer((module IntTupleMonoid), [(1, 1), (2, 2), (3, 3)]));
