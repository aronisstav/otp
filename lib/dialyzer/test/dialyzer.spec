{alias, tests, "../dialyzer_test"}.

{suites, tests, all}.

{skip_cases, tests, small_tests_SUITE, cerl_hipeify, "Needs compiler in plt"}.

{skip_cases, tests, small_tests_SUITE, [failing_setopts], "Under development (Warning not clear enough)"}.

{skip_cases, tests, small_tests_SUITE, [two_types], "Under development (Test crashes)"}.
