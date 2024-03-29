package z3

/** Configuration parameters for Z3 and [[Job]]s.
 *
 *  Use [[z3.newParams]] to create a default object followed by chains of method calls
 *  to set specific parameters. For example, to turn on statistics, set the timeout
 *  to 10 seconds, and output the interaction to a file, initialize a job as follows:
 *  {{{
 *    new Job(params = newParams.stats(true).timeout(10000))
 *  }}}
 *  For parameters that don't have a dedicated method, use `globalParam`, `contextParam`, or `solverParam`.
 */
class Parameters private[z3] (private[z3] val global: Map[String, String],
                              private[z3] val context: Map[String, String],
                              private[z3] val solverInt: Map[String, Int],
                              private[z3] val solverString: Map[String, String],
                              private[z3] val solverBool: Map[String, Boolean],
                              private[z3] val jobBool: Map[String, Boolean]) {

  /** Job parameter: allows two datatype sorts to be created with the same name. The default is
   *  `false` because it can cause confusing behavior. */
  def allow_datatype_overwrite(allow: Boolean): Parameters = clone(jobBool = jobBool + ("allow_datatype_overwrite" -> allow))

  /** Solver parameter: automatically configure solver. */
  def auto_config(on: Boolean): Parameters = clone(solverBool = solverBool + ("auto_config" -> on))

  /** Global parameter: pretty print real numbers using decimal notation (the output may be truncated).
   *  Z3 adds a ? if the value is not precise. (default: false) */
  def decimal(on: Boolean): Parameters = clone(global = global + ("pp.decimal" -> on.toString))

  /** Global parameter: maximum number of decimal places to be used when decimal=true (default: 10) */
  def decimal_precision(digits: Int): Parameters = clone(global = global + ("pp.decimal_precision" -> digits.toString))

  /** Global parameter: use real-numbered floating point literals (e.g, +1.0p-1) during pretty printing (default: false) */
  def fp_real_literals(on: Boolean): Parameters = clone(global = global + ("pp.fp_real_literals" -> on.toString))

  /** Solver parameter: enable or disable model based quantifier instantiation. */
  def mbqi(on: Boolean): Parameters = clone(solverBool = solverBool + ("smt.mbqi" -> on))

  /** Solver parameter: maximum amount of memory in megabytes */
  def max_memory(megabytes: Int): Parameters = clone(solverInt = solverInt + ("max_memory" -> megabytes))

  /** Context parameter: turn on proof generation, it must be enabled when the Z3 context is created */
  def proof(on: Boolean): Parameters = clone(context = context + ("proof" -> on.toString))

  /** Context parameter: enable statistics */
  def stats(on: Boolean): Parameters = clone(context = context + ("stats" -> on.toString))

  /** Solver parameter: file to save solver interaction */
  def smtlib2_log(filename: String): Parameters = clone(solverString = solverString + ("smtlib2_log" -> filename))

  /** Solver parameter: timeout on the solver object; overwrites a global timeout */
  def timeout(milliseconds: Int): Parameters = clone(solverInt = solverInt + ("timeout" -> milliseconds))


  /** Sets an arbitrary global parameter in this `Parameters` object. */
  def globalParam(key: String, value: String): Parameters = clone(global = global + (key -> value))

  /** Sets an arbitrary context parameter in this `Parameters` object. */
  def contextParam(key: String, value: String): Parameters = clone(context = context + (key -> value))

  /** Sets an arbitrary solver parameter in this `Parameters` object. */
  def solverParam(key: String, value: String): Parameters = clone(solverString = solverString + (key -> value))

  /** Sets an arbitrary solver parameter in this `Parameters` object. */
  def solverParam(key: String, value: Int): Parameters = clone(solverInt = solverInt + (key -> value))

  /** Sets an arbitrary solver parameter in this `Parameters` object. */
  def solverParam(key: String, value: Boolean): Parameters = clone(solverBool = solverBool + (key -> value))

  @inline private final
  def clone(global: Map[String, String] = global,
            context: Map[String, String] = context,
            solverInt: Map[String, Int] = solverInt,
            solverString: Map[String, String] = solverString,
            solverBool: Map[String, Boolean] = solverBool,
            jobBool: Map[String, Boolean] = jobBool): Parameters =
    new Parameters(global, context, solverInt, solverString, solverBool, jobBool)
}

/*
Legal global parameters are:
  bounded (bool) (default: false)
  bv_literals (bool) (default: true)
  bv_neg (bool) (default: false)
  decimal (bool) (default: false)
  decimal_precision (unsigned int) (default: 10)
  fixed_indent (bool) (default: false)
  flat_assoc (bool) (default: true)
  fp_real_literals (bool) (default: false)
  max_depth (unsigned int) (default: 5)
  max_indent (unsigned int) (default: 4294967295)
  max_num_lines (unsigned int) (default: 4294967295)
  max_ribbon (unsigned int) (default: 80)
  max_width (unsigned int) (default: 80)
  min_alias_size (unsigned int) (default: 10)
  pretty_proof (bool) (default: false)
  simplify_implies (bool) (default: true)
  single_line (bool) (default: false)

Legal context parameters are as follows. This is not consistant with param descriptions
for some reason which is very annoying.
  auto_config (bool) (default: true)
  debug_ref_count (bool) (default: false)
  dot_proof_file (string) (default: proof.dot)
  dump_models (bool) (default: false)
  encoding (string) (default: unicode)
  model (bool) (default: true)
  model_validate (bool) (default: false)
  proof (bool) (default: false)
  rlimit (unsigned int) (default: 0)
  smtlib2_compliant (bool) (default: false)
  stats (bool) (default: false)
  timeout (unsigned int) (default: 4294967295)
  trace (bool) (default: false)
  trace_file_name (string) (default: z3.log)
  type_check (bool) (default: true)
  unsat_core (bool) (default: false)
  well_sorted_check (bool) (default: false)

Legal solver parameters are as follows:
  abce (bool) (default: false)
  acce (bool) (default: false)
  add_bound_lower (rational)
  add_bound_upper (rational)
  algebraic_number_evaluator (bool) (default: true)
  anf (bool) (default: false)
  anf.delay (unsigned int) (default: 2)
  anf.exlin (bool) (default: false)
  arith.auto_config_simplex (bool) (default: false)
  arith.bprop_on_pivoted_rows (bool) (default: true)
  arith.branch_cut_ratio (unsigned int) (default: 2)
  arith.dump_lemmas (bool) (default: false)
  arith.eager_eq_axioms (bool) (default: true)
  arith.enable_hnf (bool) (default: true)
  arith.greatest_error_pivot (bool) (default: false)
  arith.ignore_int (bool) (default: false)
  arith.int_eq_branch (bool) (default: false)
  arith.min (bool) (default: false)
  arith.nl (bool) (default: true)
  arith.nl.branching (bool) (default: true)
  arith.nl.delay (unsigned int) (default: 500)
  arith.nl.expp (bool) (default: false)
  arith.nl.gr_q (unsigned int) (default: 10)
  arith.nl.grobner (bool) (default: true)
  arith.nl.grobner_cnfl_to_report (unsigned int) (default: 1)
  arith.nl.grobner_eqs_growth (unsigned int) (default: 10)
  arith.nl.grobner_expr_degree_growth (unsigned int) (default: 2)
  arith.nl.grobner_expr_size_growth (unsigned int) (default: 2)
  arith.nl.grobner_frequency (unsigned int) (default: 4)
  arith.nl.grobner_max_simplified (unsigned int) (default: 10000)
  arith.nl.grobner_subs_fixed (unsigned int) (default: 1)
  arith.nl.horner (bool) (default: true)
  arith.nl.horner_frequency (unsigned int) (default: 4)
  arith.nl.horner_row_length_limit (unsigned int) (default: 10)
  arith.nl.horner_subs_fixed (unsigned int) (default: 2)
  arith.nl.nra (bool) (default: true)
  arith.nl.order (bool) (default: true)
  arith.nl.rounds (unsigned int) (default: 1024)
  arith.nl.tangents (bool) (default: true)
  arith.print_ext_var_names (bool) (default: false)
  arith.print_stats (bool) (default: false)
  arith.propagate_eqs (bool) (default: true)
  arith.propagation_mode (unsigned int) (default: 1)
  arith.random_initial_value (bool) (default: false)
  arith.rep_freq (unsigned int) (default: 0)
  arith.simplex_strategy (unsigned int) (default: 0)
  arith.solver (unsigned int) (default: 6)
  arith_ineq_lhs (bool) (default: false)
  arith_lhs (bool) (default: false)
  array.extensional (bool) (default: true)
  array.weak (bool) (default: false)
  asymm_branch (bool) (default: true)
  asymm_branch.all (bool) (default: false)
  asymm_branch.delay (unsigned int) (default: 1)
  asymm_branch.limit (unsigned int) (default: 100000000)
  asymm_branch.rounds (unsigned int) (default: 2)
  asymm_branch.sampled (bool) (default: true)
  ate (bool) (default: true)
  auto_config (bool) (default: true)
  axioms2files (bool) (default: false)
  backtrack.conflicts (unsigned int) (default: 4000)
  backtrack.scopes (unsigned int) (default: 100)
  bca (bool) (default: false)
  bce (bool) (default: false)
  bce_at (unsigned int) (default: 2)
  bce_delay (unsigned int) (default: 2)
  binspr (bool) (default: false)
  bit2bool (bool) (default: true)
  blast_add (bool) (default: true)
  blast_distinct (bool) (default: false)
  blast_distinct_threshold (unsigned int) (default: 4294967295)
  blast_eq_value (bool) (default: false)
  blast_full (bool) (default: false)
  blast_mul (bool) (default: true)
  blast_quant (bool) (default: false)
  blast_select_store (bool) (default: false)
  blocked_clause_limit (unsigned int) (default: 100000000)
  branching.anti_exploration (bool) (default: false)
  branching.heuristic (symbol) (default: vsids)
  burst_search (unsigned int) (default: 100)
  bv.delay (bool) (default: false)
  bv.enable_int2bv (bool) (default: true)
  bv.eq_axioms (bool) (default: true)
  bv.reflect (bool) (default: true)
  bv.size_reduce (bool) (default: false)
  bv.watch_diseq (bool) (default: false)
  bv_extract_prop (bool) (default: false)
  bv_ineq_consistency_test_max (unsigned int) (default: 0)
  bv_ite2id (bool) (default: false)
  bv_le2extract (bool) (default: true)
  bv_le_extra (bool) (default: false)
  bv_not_simpl (bool) (default: false)
  bv_sort_ac (bool) (default: false)
  cache_all (bool) (default: false)
  cancel_backup_file (symbol) (default: )
  candidate_models (bool) (default: false)
  cardinality.encoding (symbol) (default: grouped)
  cardinality.solver (bool) (default: true)
  case_split (unsigned int) (default: 1)
  cce (bool) (default: false)
  check_lemmas (bool) (default: false)
  clause_proof (bool) (default: false)
  cofactor_equalities (bool)
  common_patterns (bool) (default: true)
  compact (bool) (default: true)
  compile_equality (bool)
  complete (bool) (default: true)
  completion (bool) (default: false)
  context_solve (bool) (default: false)
  core.extend_nonlocal_patterns (bool) (default: false)
  core.extend_patterns (bool) (default: false)
  core.extend_patterns.max_distance (unsigned int) (default: 4294967295)
  core.minimize (bool) (default: false)
  core.minimize_partial (bool) (default: false)
  core.validate (bool) (default: false)
  ctrl_c (bool) (default: true)
  cube_depth (unsigned int) (default: 1)
  cut (bool) (default: false)
  cut.aig (bool) (default: false)
  cut.delay (unsigned int) (default: 2)
  cut.dont_cares (bool) (default: true)
  cut.force (bool) (default: false)
  cut.lut (bool) (default: false)
  cut.npn3 (bool) (default: false)
  cut.redundancies (bool) (default: true)
  cut.xor (bool) (default: false)
  dack (unsigned int) (default: 1)
  dack.eq (bool) (default: false)
  dack.factor (double) (default: 0.1)
  dack.gc (unsigned int) (default: 2000)
  dack.gc_inv_decay (double) (default: 0.8)
  dack.threshold (unsigned int) (default: 10)
  ddfw.init_clause_weight (unsigned int) (default: 8)
  ddfw.reinit_base (unsigned int) (default: 10000)
  ddfw.restart_base (unsigned int) (default: 100000)
  ddfw.threads (unsigned int) (default: 0)
  ddfw.use_reward_pct (unsigned int) (default: 15)
  ddfw_search (bool) (default: false)
  delay_units (bool) (default: false)
  delay_units_threshold (unsigned int) (default: 32)
  dimacs.core (bool) (default: false)
  distributivity (bool) (default: true)
  distributivity_blowup (unsigned int) (default: 32)
  div0_ackermann_limit (unsigned int) (default: 1000)
  drat.activity (bool) (default: false)
  drat.binary (bool) (default: false)
  drat.check_sat (bool) (default: false)
  drat.check_unsat (bool) (default: false)
  drat.disable (bool) (default: false)
  drat.file (symbol) (default: )
  dt_lazy_splits (unsigned int) (default: 1)
  dyn_sub_res (bool) (default: true)
  elim_and (bool) (default: false)
  elim_inverses (bool) (default: true)
  elim_ite (bool) (default: true)
  elim_rem (bool) (default: false)
  elim_root_objects (bool) (default: true)
  elim_sign_ext (bool) (default: true)
  elim_to_real (bool) (default: false)
  elim_unconstrained (bool) (default: true)
  elim_vars (bool) (default: true)
  elim_vars_bdd (bool) (default: true)
  elim_vars_bdd_delay (unsigned int) (default: 3)
  ematching (bool) (default: true)
  enable_pre_simplify (bool) (default: false)
  eq2ineq (bool) (default: false)
  euf (bool) (default: false)
  expand_nested_stores (bool) (default: false)
  expand_power (bool) (default: false)
  expand_select_ite (bool) (default: false)
  expand_select_store (bool) (default: false)
  expand_store_eq (bool) (default: false)
  expand_tan (bool) (default: false)
  factor (bool)
  factor_max_prime (unsigned int) (default: 31)
  factor_num_primes (unsigned int) (default: 1)
  factor_search_size (unsigned int) (default: 5000)
  fail_if_inconclusive (bool)
  flat (bool) (default: true)
  flat_and_or (bool) (default: true)
  force_cleanup (bool) (default: false)
  gc (symbol) (default: glue_psm)
  gc.burst (bool) (default: false)
  gc.defrag (bool) (default: true)
  gc.increment (unsigned int) (default: 500)
  gc.initial (unsigned int) (default: 20000)
  gc.k (unsigned int) (default: 7)
  gc.small_lbd (unsigned int) (default: 3)
  gcd_rounding (bool) (default: false)
  hi_div0 (bool) (default: true)
  hoist_ite (bool) (default: false)
  hoist_mul (bool) (default: false)
  ignore_labels (bool) (default: false)
  ignore_patterns_on_ground_qbody (bool) (default: true)
  ignore_solver1 (bool) (default: false)
  induction (bool) (default: false)
  inline_def (bool) (default: false)
  inline_vars (bool) (default: false)
  inprocess.max (unsigned int) (default: 4294967295)
  inprocess.out (symbol) (default: )
  instantiations2console (bool) (default: false)
  ite_chaing (bool) (default: true)
  ite_extra (bool)
  ite_extra_rules (bool) (default: true)
  ite_solver (bool) (default: true)
  keep_cardinality_constraints (bool) (default: false)
  lazy (unsigned int) (default: 0)
  learned (bool)
  lemma_gc_strategy (unsigned int) (default: 0)
  lemmas2console (bool) (default: false)
  lia2pb_max_bits (unsigned int)
  lia2pb_partial (bool)
  lia2pb_total_bits (unsigned int)
  local_ctx (bool) (default: false)
  local_ctx_limit (unsigned int) (default: 4294967295)
  local_search (bool) (default: false)
  local_search_dbg_flips (bool) (default: false)
  local_search_mode (symbol) (default: wsat)
  local_search_threads (unsigned int) (default: 0)
  log_lemmas (bool) (default: false)
  logic (symbol) (default: )
  lookahead.cube.cutoff (symbol) (default: depth)
  lookahead.cube.depth (unsigned int) (default: 1)
  lookahead.cube.fraction (double) (default: 0.4)
  lookahead.cube.freevars (double) (default: 0.8)
  lookahead.cube.psat.clause_base (double) (default: 2)
  lookahead.cube.psat.trigger (double) (default: 5)
  lookahead.cube.psat.var_exp (double) (default: 1)
  lookahead.delta_fraction (double) (default: 1.0)
  lookahead.double (bool) (default: true)
  lookahead.global_autarky (bool) (default: false)
  lookahead.preselect (bool) (default: false)
  lookahead.reward (symbol) (default: march_cu)
  lookahead.use_learned (bool) (default: false)
  lookahead_scores (bool) (default: false)
  lookahead_simplify (bool) (default: false)
  lookahead_simplify.bca (bool) (default: true)
  macro_finder (bool) (default: false)
  max_args (unsigned int)
  max_conflicts (unsigned int) (default: 4294967295)
  max_degree (unsigned int) (default: 64)
  max_depth (unsigned int) (default: 1024)
  max_memory (unsigned int) (default: 4294967295)
  max_prime (unsigned int)
  max_rounds (unsigned int) (default: 4)
  max_search_size (unsigned int)
  max_steps (unsigned int) (default: 4294967295)
  mbqi (bool) (default: true)
  mbqi.force_template (unsigned int) (default: 10)
  mbqi.id (string) (default: )
  mbqi.max_cexs (unsigned int) (default: 1)
  mbqi.max_cexs_incr (unsigned int) (default: 0)
  mbqi.max_iterations (unsigned int) (default: 1000)
  mbqi.trace (bool) (default: false)
  min_mag (unsigned int) (default: 16)
  minimize_conflicts (bool) (default: false)
  minimize_lemmas (bool) (default: true)
  mode (symbol) (default: skolem)
  model (bool) (default: true)
  mul2concat (bool) (default: false)
  mul_to_power (bool) (default: false)
  nla2bv_bv_size (unsigned int) (default: 4)
  nla2bv_divisor (unsigned int) (default: 2)
  nla2bv_max_bv_size (unsigned int)
  nla2bv_root (unsigned int) (default: 2)
  norm_int_only (bool) (default: true)
  num_primes (unsigned int)
  override_incremental (bool) (default: false)
  partial (bool) (default: false)
  pb.conflict_frequency (unsigned int) (default: 1000)
  pb.learn_complements (bool) (default: true)
  pb.lemma_format (symbol) (default: cardinality)
  pb.min_arity (unsigned int) (default: 9)
  pb.resolve (symbol) (default: cardinality)
  pb.solver (symbol) (default: solver)
  pb2bv_all_clauses_limit (unsigned int)
  pb2bv_cardinality_limit (unsigned int)
  phase (symbol) (default: caching)
  phase.sticky (bool) (default: true)
  phase_caching_off (unsigned int) (default: 100)
  phase_caching_on (unsigned int) (default: 400)
  phase_selection (unsigned int) (default: 3)
  prob_search (bool) (default: false)
  probing (bool) (default: true)
  probing_binary (bool) (default: true)
  probing_cache (bool) (default: true)
  probing_cache_limit (unsigned int) (default: 1024)
  probing_limit (unsigned int) (default: 5000000)
  produce_models (bool) (default: false)
  proof (bool) (default: false)
  proof.check (bool) (default: true)
  proof.log (symbol) (default: )
  proof.save (bool) (default: false)
  proof.trim (bool) (default: false)
  propagate.prefetch (bool) (default: true)
  propagate_eq (bool) (default: false)
  propagate_values (bool) (default: true)
  pull_cheap_ite (bool) (default: false)
  pull_nested_quantifiers (bool) (default: false)
  push_ite_arith (bool) (default: false)
  push_ite_bv (bool) (default: false)
  push_to_real (bool) (default: true)
  q.lift_ite (unsigned int) (default: 0)
  q.lite (bool) (default: false)
  qi.cost (string) (default: (+ weight generation))
  qi.eager_threshold (double) (default: 10.0)
  qi.lazy_threshold (double) (default: 20.0)
  qi.max_instances (unsigned int) (default: 4294967295)
  qi.max_multi_patterns (unsigned int) (default: 0)
  qi.profile (bool) (default: false)
  qi.profile_freq (unsigned int) (default: 4294967295)
  qi.quick_checker (unsigned int) (default: 0)
  quasi_macros (bool) (default: false)
  random_freq (double) (default: 0.01)
  random_seed (unsigned int) (default: 0)
  randomize (bool) (default: true)
  refine_inj_axioms (bool) (default: true)
  relevancy (unsigned int) (default: 2)
  reorder (bool) (default: true)
  reorder.activity_scale (unsigned int) (default: 100)
  reorder.base (unsigned int) (default: 4294967295)
  reorder.itau (double) (default: 4.0)
  rephase.base (unsigned int) (default: 1000)
  resolution.cls_cutoff1 (unsigned int) (default: 100000000)
  resolution.cls_cutoff2 (unsigned int) (default: 700000000)
  resolution.limit (unsigned int) (default: 500000000)
  resolution.lit_cutoff_range1 (unsigned int) (default: 700)
  resolution.lit_cutoff_range2 (unsigned int) (default: 400)
  resolution.lit_cutoff_range3 (unsigned int) (default: 300)
  resolution.occ_cutoff (unsigned int) (default: 10)
  resolution.occ_cutoff_range1 (unsigned int) (default: 8)
  resolution.occ_cutoff_range2 (unsigned int) (default: 5)
  resolution.occ_cutoff_range3 (unsigned int) (default: 3)
  restart (symbol) (default: ema)
  restart.emafastglue (double) (default: 0.03)
  restart.emaslowglue (double) (default: 1e-05)
  restart.factor (double) (default: 1.5)
  restart.fast (bool) (default: true)
  restart.initial (unsigned int) (default: 2)
  restart.margin (double) (default: 1.1)
  restart.max (unsigned int) (default: 4294967295)
  restart_factor (double) (default: 1.1)
  restart_strategy (unsigned int) (default: 1)
  restricted_quasi_macros (bool) (default: false)
  retain_blocked_clauses (bool) (default: true)
  rewrite_patterns (bool) (default: false)
  rlimit (unsigned int) (default: 0)
  scc (bool) (default: true)
  scc.tr (bool) (default: true)
  search.sat.conflicts (unsigned int) (default: 400)
  search.unsat.conflicts (unsigned int) (default: 400)
  seed (unsigned int) (default: 0)
  seq.max_unfolding (unsigned int) (default: 1000000000)
  seq.min_unfolding (unsigned int) (default: 1)
  seq.split_w_len (bool) (default: true)
  seq.validate (bool) (default: false)
  shuffle_vars (bool) (default: false)
  simplify.delay (unsigned int) (default: 0)
  simplify_conflicts (bool) (default: true)
  sk_hack (bool) (default: false)
  smt (bool) (default: false)
  smt.proof.check (bool) (default: false)
  smt.proof.check_rup (bool) (default: true)
  smtlib2_log (symbol) (default: )
  solve_eqs (bool) (default: true)
  solve_eqs_max_occs (unsigned int) (default: 4294967295)
  solver2_timeout (unsigned int) (default: 4294967295)
  solver2_unknown (unsigned int) (default: 1)
  som (bool) (default: false)
  som_blowup (unsigned int) (default: 10)
  sort_store (bool) (default: false)
  sort_sums (bool) (default: false)
  split_concat_eq (bool) (default: false)
  split_factors (bool) (default: true)
  str.aggressive_length_testing (bool) (default: false)
  str.aggressive_unroll_testing (bool) (default: true)
  str.aggressive_value_testing (bool) (default: false)
  str.fast_length_tester_cache (bool) (default: false)
  str.fast_value_tester_cache (bool) (default: true)
  str.fixed_length_naive_cex (bool) (default: true)
  str.fixed_length_refinement (bool) (default: false)
  str.overlap_priority (double) (default: -0.1)
  str.regex_automata_difficulty_threshold (unsigned int) (default: 1000)
  str.regex_automata_failed_automaton_threshold (unsigned int) (default: 10)
  str.regex_automata_failed_intersection_threshold (unsigned int) (default: 10)
  str.regex_automata_intersection_difficulty_threshold (unsigned int) (default: 1000)
  str.regex_automata_length_attempt_threshold (unsigned int) (default: 10)
  str.string_constant_cache (bool) (default: true)
  str.strong_arrangements (bool) (default: true)
  string_solver (symbol) (default: seq)
  subsumption (bool) (default: true)
  subsumption.limit (unsigned int) (default: 100000000)
  theory_aware_branching (bool) (default: false)
  theory_case_split (bool) (default: false)
  theory_solver (bool) (default: true)
  threads (unsigned int) (default: 1)
  threads.cube_frequency (unsigned int) (default: 2)
  threads.max_conflicts (unsigned int) (default: 400)
  timeout (unsigned int) (default: 4294967295)
  unsat_core (bool) (default: false)
  user_functions (bool) (default: true)
  v1 (bool) (default: false)
  v2 (bool) (default: false)
  variable_decay (unsigned int) (default: 110)
  zero_accuracy (unsigned int) (default: 0)
 */