open Interp2
open OUnit2
let desugar_tests =
  "desugar test suite" >:::
    [
      "basic test" >:: (fun _ ->
        let prog =
          Option.get
            (parse
               "let id (x : int) : int = x
                let y : int = 5")
        in
        let expected =
          Let
            {
              is_rec = false;
              name = "id";
              ty = FunTy (IntTy, IntTy);
              binding = Fun ("x", IntTy, Var "x");
              body = Let
                       {
                         is_rec = false;
                         name = "y";
                         ty = IntTy;
                         binding = Num 5;
                         body = Var "y";
                       };
            }
        in
        let actual = desugar prog in
        assert_equal expected actual);

      "Desugaring written" >:: (fun _ ->
        let prog =
          Option.get
            (parse
               "let foo (x : int) (y : int) : int =
                let bar (z : bool) : bool = z || x = y in
                bar true
                let baz (x : unit) : int = foo 1 2
                let biz : int = baz ()")
        in
        let expected =
          Let
            {
              is_rec = false;
              name = "foo";
              ty = FunTy (IntTy, FunTy (IntTy, IntTy));
              binding = Fun ("x", IntTy, Fun ("y", IntTy,
                          Let
                            {
                              is_rec = false;
                              name = "bar";
                              ty = FunTy (BoolTy, BoolTy);
                              binding = Fun ("z", BoolTy, Bop (Or, Var "z", Bop (Eq, Var "x", Var "y")));
                              body = App (Var "bar", Bool true);
                            }));
              body = Let
                       {
                         is_rec = false;
                         name = "baz";
                         ty = FunTy (UnitTy, IntTy);
                         binding = Fun ("x", UnitTy, App (App (Var "foo", Num 1), Num 2));
                         body = Let
                                  {
                                    is_rec = false;
                                    name = "biz";
                                    ty = IntTy;
                                    binding = App (Var "baz", Unit);
                                    body = Var "biz";
                                  };
                       };
            }
        in
        let actual = desugar prog in
        assert_equal expected actual);
    ]

let type_of_tests =
  "type_of test suite" >:::
    [
      "basic test" >:: (fun _ ->
        let expr = Fun ("x", BoolTy, Num 5) in
        let expected = Ok (FunTy (BoolTy, IntTy)) in
        let actual = type_of expr in
        assert_equal expected actual
      );
      (* TODO: write more tests *)
    ]

    let eval_tests =
      "eval test suite" >:::
        [
          "simple recursive function" >:: (fun _ ->
            let prog = Option.get (parse "let rec f (n : int) : int = if n <= 0 then 0 else f (n-1)\nlet result : int = f 3") in
            let expr = desugar prog in
            let expected = VNum 0 in
            let actual = eval expr in
            assert_equal expected actual
          );
          "fibonacci" >:: (fun _ ->
            let prog = Option.get (parse "let rec fib (n : int) : int = if n <= 1 then n else fib (n-1) + fib (n-2)\nlet _ : unit = assert (fib 5 = 5)") in
            let expr = desugar prog in
            let expected = VUnit in
            let actual = eval expr in
            assert_equal expected actual
          );
          "nested recursive calls" >:: (fun _ ->
            let prog = Option.get (parse "let rec fact (n : int) : int = if n <= 1 then 1 else n * fact (n-1)\nlet _ : unit = assert (fact 5 = 120)") in
            let expr = desugar prog in
            let expected = VUnit in
            let actual = eval expr in
            assert_equal expected actual
          );
        ]


let tests =
  "interp2 test suite" >:::
    [
      desugar_tests;
      type_of_tests;
      eval_tests
    ]

let _ = run_test_tt_main tests
