open! Core

module Stable = struct
  open Stable_witness.Export

  module V1 = struct
    module T = struct
      type t =
        { year : (int[@stable])
        ; week_number : (int[@stable])
        }
      [@@deriving
        bin_io
        , compare ~localize
        , equal ~localize
        , fields ~getters
        , hash
        , stable_witness
        , trace_types ~portable]

      let weeks_per_year year =
        (* This calculation is from
           https://en.wikipedia.org/wiki/ISO_week_date#Weeks_per_year

           TL;DR: If 1 Jan or 31 Dec are Thursdays then the year has 53 weeks, otherwise
           it has 52 weeks. *)
        match
          Day_of_week.(
            Date.day_of_week (Date.create_exn ~y:year ~m:Jan ~d:1) = Thu
            || Date.day_of_week (Date.create_exn ~y:year ~m:Dec ~d:31) = Thu)
        with
        | true -> 53
        | false -> 52
      ;;

      let validate { week_number; year } =
        Int.between ~low:1 ~high:(weeks_per_year year) week_number
        && Int.between ~low:1900 ~high:2100 year
      ;;

      let to_string t =
        let module Digit_string_helpers = Core.Core_private.Digit_string_helpers in
        assert (validate t);
        let bytes = Bytes.create 8 in
        Digit_string_helpers.write_4_digit_int bytes ~pos:0 t.year;
        Bytes.set bytes 4 '-';
        Bytes.set bytes 5 'W';
        Digit_string_helpers.write_2_digit_int bytes ~pos:6 t.week_number;
        Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes
      ;;

      let of_string s =
        let module Digit_string_helpers = Core.Core_private.Digit_string_helpers in
        let parse ~year:year_pos ~week:week_pos =
          let t =
            { year = Digit_string_helpers.read_4_digit_int s ~pos:year_pos
            ; week_number = Digit_string_helpers.read_2_digit_int s ~pos:week_pos
            }
          in
          assert (validate t);
          t
        in
        try
          let has_iso_week_shape =
            String.length s = 8
            && Char.( = ) (String.get s 4) '-'
            && Char.( = ) (String.get s 5) 'W'
          in
          match has_iso_week_shape with
          | true -> parse ~year:0 ~week:6
          | false -> failwith "Year_week.of_string: not of the form YYYY-Www"
        with
        | _ ->
          failwithf
            "Year_week.of_string: expected ISO week number of the form 2001-W23, got '%s'"
            s
            ()
      ;;
    end

    include T
    include Sexpable.Stable.Of_stringable.V1 [@modality portable] (T)
    include (val (Comparator.Stable.V1.make [@mode portable]) ~compare ~sexp_of_t)

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| c9aca48bcc0c06aade6fb5d32b5368ac |}]
    ;;

    let%expect_test _ =
      print_endline [%sexp_digest: t];
      [%expect {| 27118326006d3829667a400ad23d5d98 |}]
    ;;
  end
end

include Stable.V1
include functor Comparable.Make_plain_using_comparator [@modality portable]

let create_exn ~week_number ~year =
  let t = { week_number; year } in
  assert (validate t);
  t
;;

let quickcheck_generator : t Quickcheck.Generator.t =
  let%bind.Base_quickcheck.Generator.Portable year = Int.gen_incl 1900 2100 in
  let%bind.Base_quickcheck.Generator.Portable week_number =
    Int.gen_incl 1 (weeks_per_year year)
  in
  Base_quickcheck.Generator.Portable.Let_syntax.return { week_number; year }
;;

let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic

let of_date date =
  let week_number, year = Date.week_number_and_year date in
  { week_number; year }
;;

let to_date { week_number; year } ~day_of_week =
  (* From
     https://en.wikipedia.org/wiki/ISO_week_date#Calculating_an_ordinal_or_month_date_from_a_week_date

     d = week * 7 + day - (day_of_week(4th Jan) + 3)
  *)
  let jan_4 =
    Date.create_exn ~y:year ~m:Jan ~d:4
    |> Date.day_of_week
    |> Day_of_week.iso_8601_weekday_number
  in
  let day_of_week = Day_of_week.iso_8601_weekday_number day_of_week in
  let d = (week_number * 7) + day_of_week - (jan_4 + 3) in
  Date.add_days (Date.create_exn ~y:year ~m:Jan ~d:1) (d - 1)
;;

let%expect_test "quickcheck_generator only generates valid instances" =
  Quickcheck.test [%quickcheck.generator: t] ~sexp_of:[%sexp_of: t] ~f:(fun year_week ->
    assert (validate year_week))
;;

let%expect_test "to_date (of_date date) = date" =
  Quickcheck.test
    [%quickcheck.generator: Date.t]
    ~sexp_of:[%sexp_of: Date.t]
    ~f:(fun date ->
      let day_of_week = Date.day_of_week date in
      let year_week = of_date date in
      let result_date = to_date year_week ~day_of_week in
      [%test_result: Date.t] result_date ~expect:date)
;;

let%expect_test "of_date (to_date (year_week)) = year_week" =
  Quickcheck.test
    [%quickcheck.generator: t * Day_of_week.t]
    ~sexp_of:[%sexp_of: t * Day_of_week.t]
    ~f:(fun (year_week, day_of_week) ->
      let date_commencing = to_date year_week ~day_of_week in
      let result_year_week = of_date date_commencing in
      [%test_result: t] result_year_week ~expect:year_week)
;;

let%expect_test "to_string" =
  let test t = print_endline (to_string t) in
  test { year = 1900; week_number = 1 };
  [%expect {| 1900-W01 |}];
  test { year = 2001; week_number = 23 };
  [%expect {| 2001-W23 |}];
  test { year = 2100; week_number = 52 };
  [%expect {| 2100-W52 |}]
;;

let%expect_test "of_string (to_string (year_week)) = year_week" =
  Quickcheck.test [%quickcheck.generator: t] ~sexp_of:[%sexp_of: t] ~f:(fun year_week ->
    let year_week_str = to_string year_week in
    let result_year_week = of_string year_week_str in
    [%test_result: t] result_year_week ~expect:year_week)
;;

let first_date_in_week t = to_date t ~day_of_week:Mon
let last_date_in_week t = to_date t ~day_of_week:Sun

let%expect_test "first_date_in_week" =
  Quickcheck.test
    [%quickcheck.generator: t * Day_of_week.t]
    ~sexp_of:[%sexp_of: t * Day_of_week.t]
    ~f:(fun (t, day_of_week) ->
      let date_in_week = to_date t ~day_of_week in
      let first_date_in_week = first_date_in_week t in
      assert (Date.( <= ) first_date_in_week date_in_week))
;;

let%expect_test "last_date_in_week" =
  Quickcheck.test
    [%quickcheck.generator: t * Day_of_week.t]
    ~sexp_of:[%sexp_of: t * Day_of_week.t]
    ~f:(fun (t, day_of_week) ->
      let date_in_week = to_date t ~day_of_week in
      let last_date_in_week = last_date_in_week t in
      assert (Date.( <= ) date_in_week last_date_in_week))
;;
