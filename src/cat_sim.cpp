#include <Rcpp.h>
#include <chrono>
#include <ctime> // put_time
#include "cat_select_next_item.h"
#include "cat_sim_functions.h"

// #include <Rcpp/Benchmark/Timer.h>
using namespace Rcpp;


//#############################################################################@
//########################### cat_sim_single_cpp ###############################
//#############################################################################@
// Run CAT simulation for one examinee.
// [[Rcpp::export]]
Rcpp::List cat_sim_single_cpp(Rcpp::List true_ability, Rcpp::List cd,
                              Rcpp::String examinee_id = NA_STRING) {
  // Rcout << "Stage - Beginning" << std::endl;
  // TODO: The following line assumes theta is a vector of length one. For MIRT
  // or other models the following needs to be changed.


  // Timer timer;           // start the timer
  // timer.step("Starting Function  ");   // record the starting point

  int max_test_length = cd["max_test_length"];
  Rcpp::List output = Rcpp::List::create(
    Rcpp::Named("examinee_id") = examinee_id,
    Rcpp::Named("true_ability") = true_ability,
    Rcpp::Named("est_history") = Rcpp::List(),
    Rcpp::Named("additional_args") = Rcpp::List());

  // Rcout << "(cat_sim_single_cpp) - Initial Ability Estimate 2" << std::endl;

  // List that holds the result of a step
  Rcpp::List temp_result = next_step_cat_cpp(true_ability, cd, R_NilValue,
                                             R_NilValue);
  Rcpp::List est_history = temp_result["est_history"];
  Rcpp::List additional_args = temp_result["additional_args"];

  //////////  Run Simulations //////////
  // Rcout << "(cat_sim_single_cpp) - Run Simulations" << std::endl;
  for (int i = 0; i <= max_test_length; i++) {
    // Rcout <<  std::endl << "  ##################  Sim -- Beginning - " << i << " ###########" << std::endl;

    // timer.step("\n--- Start Step " + std::to_string(i) + " -- ");

    ///// Select Next Item /////
    // Rcout << "  (cat_sim_single_cpp)  -- Sim -- Select Next Item "<< std::endl;
    temp_result = select_next_item_cpp(cd, est_history, additional_args);
    est_history = temp_result["est_history"];
    additional_args = temp_result["additional_args"];

    // timer.step("Next item Selected  ");

    ///// Generate Response /////
    // Rcout << "  (cat_sim_single_cpp)  -- Sim -- Generate Response "<< std::endl;
    temp_result = generate_cat_resp_cpp(true_ability, cd, est_history,
                                        additional_args);
    est_history = temp_result["est_history"];
    additional_args = temp_result["additional_args"];

    // timer.step("Response Generated  ");

    ///// Estimate Ability /////
    // Rcout << "  (cat_sim_single_cpp)  -- Sim -- Estimate Ability "<< std::endl;
    temp_result = est_ability_cat_cpp(true_ability, cd, est_history,
                                      additional_args, false);
    est_history = temp_result["est_history"];
    additional_args = temp_result["additional_args"];

    // timer.step("Ability Estimated   ");

    ///// Terminate Test? /////
    // Rcout << "  (cat_sim_single_cpp)  -- Sim -- Terminate Test = " << terminate_test << std::endl;
    if (terminate_cat_cpp(true_ability, cd, est_history, additional_args)) {
      // Estimate the final ability estimate if there is a rule specified.
      if (cd.containsElementNamed("final_ability_est_rule") &&
          !Rf_isNull(cd("final_ability_est_rule"))) {
            // set last_estimate to 'true'
            temp_result = est_ability_cat_cpp(
              true_ability, cd, est_history, additional_args, true);
            est_history = temp_result["est_history"];
            additional_args = temp_result["additional_args"];
      }
      break;
    } else {
      // Create a new est_history step:
      temp_result = next_step_cat_cpp(true_ability, cd, est_history,
                                      additional_args);
      est_history = temp_result["est_history"];
      additional_args = temp_result["additional_args"];
    }
    // timer.step("Finishing Iterations");
  }

  // timer.step("Finishing Simulation for one person");  // record the second step
  // NumericVector res(timer);
  // StringVector res_names = res.names();
  // for (int i=0; i<res.size(); i++) {
        // Rcout << res_names[i] << ": " << res[i]/10000000 << std::endl;
    // }

  // Rcout << "(cat_sim_single_cpp)  -- Finishing Simulation" << std::endl;
  output("est_history") = est_history;
  output("additional_args") = additional_args;
  output.attr("class") = "cat_output";
  return output;
}


void print_cat_progress(int i, int n_sim) {
  auto now = std::chrono::system_clock::to_time_t(std::chrono::system_clock::now());
  std::string s(20, '\0');
  std::strftime(&s[0], s.size(), "%H:%M:%S", std::localtime(&now));
  Rcout << "cat_sim - " << i << "/" << n_sim << " - " << s <<  std::endl;
    //std::put_time(localtime(&now), "%H:%M:%S") <<  std::endl;
}

//#############################################################################@
//########################### cat_sim_cpp ######################################
//#############################################################################@
// [[Rcpp::export]]
Rcpp::List cat_sim_cpp(Rcpp::List true_ability, Rcpp::List cd, int verbose = 0)
{
  int n_sim = true_ability.size();
  Rcpp::List output(n_sim);
  Rcpp::List temp_list;
  Rcpp::CharacterVector examinee_ids(n_sim);

  if (true_ability.hasAttribute("names")) {
    examinee_ids = true_ability.attr("names");
  } else {
    for(int i = 0; i < n_sim; i++){
      examinee_ids[i] = "S" + std::to_string(i+1);
    }
  }

  if (verbose > 0) print_cat_progress(0, n_sim);
  try
  {
    if (cd.inherits("cat_design")) {
      for (int i = 0; i < n_sim; i++) {
        // The following line asssumes each element of true_ability is a single
        // numeric variable. For MIRT or CDM-CAT it will not work.
        output[i] = cat_sim_single_cpp(true_ability[i], cd, examinee_ids[i]);
        if (verbose > 0 && (i+1) % verbose == 0) print_cat_progress(i+1, n_sim);
        if(i % 5 == 0) Rcpp::checkUserInterrupt();
      }
    } else {
      // In case there is a separate cat_design for each examinee, run this.
      // This part assumes that cat_sim_cpp function is accessed via cat_sim()
      // function where cat_sim() function already checks whether each element
      // of the list is a 'cat_design' object and the length of the list is
      // equal to the length of true_ability. If not, this function will fail.
      for (int i = 0; i < n_sim; i++) {
        // The following line asssumes each element of true_ability is a single
        // numeric variable. For MIRT or CDM-CAT it will not work.
        output[i] = cat_sim_single_cpp(true_ability[i], cd[i], examinee_ids[i]);
        if (verbose > 0 && (i+1) % verbose == 0) print_cat_progress(i+1, n_sim);
        if(i % 5 == 0) Rcpp::checkUserInterrupt();
      }
    }
  } catch (Rcpp::internal::InterruptedException& e)
  {
    Rcout << "cat_sim interrupted by the user!" << std::endl;
  }

  // if true_ability vector/list has names, set the names for output as well
  output.attr("names") = examinee_ids;

  if (n_sim == 1) {
    return output[0];
  } else return output;
}



