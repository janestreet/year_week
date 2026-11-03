@@ portable

(** Implements the year and week specification from ISO 8601 week-dates [1], which are the
    output of [Date.week_number_and_year].

    [1] : https://en.wikipedia.org/wiki/ISO_week_date *)

open! Core

type t : immutable_data
[@@deriving
  compare ~localize
  , equal ~localize
  , hash
  , sexp_of
  , to_string
  , trace_types
  , quickcheck ~generator ~shrinker]

include Comparable.S_plain [@modality portable] with type t := t

val create_exn : week_number:int -> year:int -> t
val week_number : t -> int
val year : t -> int
val of_date : Date.t -> t
val to_date : t -> day_of_week:Day_of_week.t -> Date.t

(** By definition, ISO weeks start on Monday. *)
val first_date_in_week : t -> Date.t

(** By definition, ISO weeks end on Sunday. *)
val last_date_in_week : t -> Date.t

module Stable : sig
  module V1 : sig
    type nonrec t = t
    [@@deriving
      bin_io
      , compare ~localize
      , equal ~localize
      , hash
      , sexp
      , sexp_grammar
      , stable_witness
      , string
      , trace_types]

    include
      Core_stable.Comparator.V1.S
      [@modality portable]
      with type t := t
       and type comparator_witness = comparator_witness
  end
end
