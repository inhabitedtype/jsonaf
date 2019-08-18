open Core

let test_file_dir = "json"

let%expect_test _ =
  let test_files = Sys.ls_dir test_file_dir in
  let results =
    List.map test_files ~f:(fun test_file_name ->
        let file_contents =
          In_channel.read_all (test_file_dir ^/ test_file_name)
        in
        let found =
          match Jsonaf.of_string file_contents with
          | Ok _ -> `Yes
          | Error err -> `No err
        in
        let expected =
          match String.get test_file_name 0 with
          | 'y' -> `Yes
          | 'n' -> `No
          | 'i' -> `Implementation_defined
          | _ ->
            raise_s
              [%message "not sure what to expect from test file" (test_file_name : string)]
        in
        let passed =
          match (expected, found) with
          | `Yes, `Yes | `No, `No _ | `Implementation_defined, _ -> true
          | `Yes, `No _ | `No, `Yes -> false
        in
        (test_file_name, found, expected, passed))
  in
  List.iter results ~f:(fun (file_name, found, expected, passed) ->
      if not passed then
        printf
          !"%s: expected %{sexp: [`Yes | `No | `Implementation_defined]}, found: %{sexp: [`Yes | `No of string]}\n"
          file_name
          expected
          found);
  [%expect {|
    n_array_a_invalid_utf8.json: expected No, found: Yes
    n_array_comma_after_close.json: expected No, found: Yes
    n_array_extra_close.json: expected No, found: Yes
    n_array_invalid_utf8.json: expected No, found: Yes
    n_array_just_minus.json: expected No, found: Yes
    n_array_star_inside.json: expected No, found: Yes
    n_multidigit_number_then_00.json: expected No, found: Yes
    n_number_++.json: expected No, found: Yes
    n_number_+1.json: expected No, found: Yes
    n_number_+Inf.json: expected No, found: Yes
    n_number_-01.json: expected No, found: Yes
    n_number_-1.0..json: expected No, found: Yes
    n_number_-2..json: expected No, found: Yes
    n_number_-NaN.json: expected No, found: Yes
    n_number_.-1.json: expected No, found: Yes
    n_number_.2e-3.json: expected No, found: Yes
    n_number_0.1.2.json: expected No, found: Yes
    n_number_0.3e+.json: expected No, found: Yes
    n_number_0.3e.json: expected No, found: Yes
    n_number_0.e1.json: expected No, found: Yes
    n_number_0_capital_E+.json: expected No, found: Yes
    n_number_0_capital_E.json: expected No, found: Yes
    n_number_0e+.json: expected No, found: Yes
    n_number_0e.json: expected No, found: Yes
    n_number_1.0e+.json: expected No, found: Yes
    n_number_1.0e-.json: expected No, found: Yes
    n_number_1.0e.json: expected No, found: Yes
    n_number_1eE2.json: expected No, found: Yes
    n_number_2.e+3.json: expected No, found: Yes
    n_number_2.e-3.json: expected No, found: Yes
    n_number_2.e3.json: expected No, found: Yes
    n_number_9.e+.json: expected No, found: Yes
    n_number_Inf.json: expected No, found: Yes
    n_number_NaN.json: expected No, found: Yes
    n_number_U+FF11_fullwidth_digit_one.json: expected No, found: Yes
    n_number_expression.json: expected No, found: Yes
    n_number_hex_1_digit.json: expected No, found: Yes
    n_number_hex_2_digits.json: expected No, found: Yes
    n_number_infinity.json: expected No, found: Yes
    n_number_invalid+-.json: expected No, found: Yes
    n_number_invalid-negative-real.json: expected No, found: Yes
    n_number_invalid-utf-8-in-bigger-int.json: expected No, found: Yes
    n_number_invalid-utf-8-in-exponent.json: expected No, found: Yes
    n_number_invalid-utf-8-in-int.json: expected No, found: Yes
    n_number_minus_infinity.json: expected No, found: Yes
    n_number_minus_sign_with_trailing_garbage.json: expected No, found: Yes
    n_number_neg_int_starting_with_zero.json: expected No, found: Yes
    n_number_neg_real_without_int_part.json: expected No, found: Yes
    n_number_neg_with_garbage_at_end.json: expected No, found: Yes
    n_number_real_garbage_after_e.json: expected No, found: Yes
    n_number_real_with_invalid_utf8_after_e.json: expected No, found: Yes
    n_number_real_without_fractional_part.json: expected No, found: Yes
    n_number_starting_with_dot.json: expected No, found: Yes
    n_number_with_alpha.json: expected No, found: Yes
    n_number_with_alpha_char.json: expected No, found: Yes
    n_number_with_leading_zero.json: expected No, found: Yes
    n_object_trailing_comment.json: expected No, found: Yes
    n_object_trailing_comment_open.json: expected No, found: Yes
    n_object_trailing_comment_slash_open.json: expected No, found: Yes
    n_object_trailing_comment_slash_open_incomplete.json: expected No, found: Yes
    n_object_with_trailing_garbage.json: expected No, found: Yes
    n_string_accentuated_char_no_quotes.json: expected No, found: Yes
    n_string_leading_uescaped_thinspace.json: expected No, found: Yes
    n_string_no_quotes_with_bad_escape.json: expected No, found: Yes
    n_string_single_string_no_double_quotes.json: expected No, found: Yes
    n_string_with_trailing_garbage.json: expected No, found: Yes
    n_structure_U+2060_word_joined.json: expected No, found: Yes
    n_structure_UTF8_BOM_no_data.json: expected No, found: Yes
    n_structure_angle_bracket_..json: expected No, found: Yes
    n_structure_angle_bracket_null.json: expected No, found: Yes
    n_structure_array_trailing_garbage.json: expected No, found: Yes
    n_structure_array_with_extra_array_close.json: expected No, found: Yes
    n_structure_ascii-unicode-identifier.json: expected No, found: Yes
    n_structure_capitalized_True.json: expected No, found: Yes
    n_structure_close_unopened_array.json: expected No, found: Yes
    n_structure_double_array.json: expected No, found: Yes
    n_structure_incomplete_UTF8_BOM.json: expected No, found: Yes
    n_structure_lone-invalid-utf-8.json: expected No, found: Yes
    n_structure_null-byte-outside-string.json: expected No, found: Yes
    n_structure_number_with_trailing_garbage.json: expected No, found: Yes
    n_structure_object_followed_by_closing_object.json: expected No, found: Yes
    n_structure_object_with_comment.json: expected No, found: Yes
    n_structure_object_with_trailing_garbage.json: expected No, found: Yes
    n_structure_single_eacute.json: expected No, found: Yes
    n_structure_single_star.json: expected No, found: Yes
    n_structure_trailing_#.json: expected No, found: Yes
    n_structure_uescaped_LF_before_string.json: expected No, found: Yes
    n_structure_unicode-identifier.json: expected No, found: Yes
    n_structure_whitespace_U+2060_word_joiner.json: expected No, found: Yes
    n_structure_whitespace_formfeed.json: expected No, found: Yes |}]
