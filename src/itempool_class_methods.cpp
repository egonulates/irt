#include <Rcpp.h>
#include "misc.h"
using namespace Rcpp;





//##############################################################################
//##############################################################################
//##############################################################################
//##############################################################################
//##############################################################################
//########################### item-Class-Methods ###############################
//##############################################################################
//##############################################################################
//##############################################################################
//##############################################################################
//##############################################################################



//#############################################################################@
//########################### get_itempool_size ################################
//#############################################################################@
//' Get the length of an item pool
//'
//' @description This function gets length of an item pool from three different
//'   aspects.
//' @param ip An \code{\link{Itempool-class}} object.
//' @return This vector will return three numbers:
//' "elements": The number of items (excluding the ones in testlets) and
//'   testlets.
//' "testlets": The number of testlets
//' "items": The number of items including the ones in testlets. But this
//'   number excludes the testlets. It is basically the possible number of
//'   responses from an item pool.
//'
//' @noRd
//'
// [[Rcpp::export]]
Rcpp::IntegerVector get_itempool_size(Rcpp::S4 ip) {
  if (!ip.inherits("Itempool"))
    stop("Please provide a valid 'Itempool' object.");
  Rcpp::List ip_list = ip.slot("item_list");
  int no_of_elements = ip_list.size();
  int no_of_testlets = 0;
  int no_of_items = 0;
  // this is temporary element to hold either item or testlet
  S4 temp_element, temp_ip;
  List temp_ip_list;
  for (int i = 0; i < no_of_elements; i++) {
    temp_element = as<S4>(ip_list[i]);
    if (temp_element.inherits("Testlet")) {
      no_of_testlets++;
      temp_ip = as<S4>(temp_element.slot("item_list"));
      temp_ip_list = temp_ip.slot("item_list");
      no_of_items = no_of_items + temp_ip_list.size();
    } else  {
      no_of_items++;
    }
  }

  return IntegerVector::create(Named("elements") = no_of_elements,
                               Named("testlets") = no_of_testlets,
                               Named("items") = no_of_items);
}



//#############################################################################@
//########################### get_ids_itempool_cpp #############################
//#############################################################################@
//' Extract a string slot of an \code{\link{Itempool-class}} object.
//'
//' @description This function extracts the slot all \code{\link{Item-class}}
//'   objects within an \code{\link{Itempool-class}} object. Note that slot
//'   should hold a character class value.
//' @param ip An \code{\link{Itempool-class}} object.
//' @return A string vector that holds the values of extracted slot.
//'
//' @noRd
//'
// [[Rcpp::export]]
Rcpp::StringVector get_ids_itempool_cpp(Rcpp::S4 ip)
{
  Rcpp::S4 tempS4;
  Rcpp::List item_list = ip.slot("item_list");
  int noItem = item_list.size();
  Rcpp::StringVector output(noItem);
  for (int i = 0; i < noItem; i++)
  {
    tempS4 = as<Rcpp::S4>(item_list[i]);
    if (tempS4.inherits("Testlet")) {
      output[i] = as<std::string>(tempS4.slot("testlet_id"));
    } else {
      output[i] = as<std::string>(tempS4.slot("item_id"));
    }
  }
  return(output);
}



//#############################################################################@
//########################### get_item_ids_itempool_cpp ########################
//#############################################################################@
//' Extract item ids of \code{\link{Itempool-class}} object.
//'
//' @description This function extracts item id's of item pool. If the item pool
//'   has testlets, it extracts the items within the testlet object. 
//' @param ip An \code{\link{Itempool-class}} object.
//' @return A string vector that holds the values of item IDs.
//'
//' @noRd
//'
// [[Rcpp::export]]
Rcpp::StringVector get_item_ids_itempool_cpp(Rcpp::S4 ip)
{
  Rcpp::S4 tempS4, temp_item;
  Rcpp::List item_list = ip.slot("item_list");
  Rcpp::List temp_item_list;
  Rcpp::IntegerVector ip_size = get_itempool_size(ip);
  int n_item = ip_size[2];
  int n_elements = item_list.size();
  Rcpp::StringVector output(n_item);
  int counter = 0;
  for (int i = 0; i < n_elements; i++)
  {
    tempS4 = as<Rcpp::S4>(item_list[i]);
    if (tempS4.inherits("Testlet")) {
      tempS4 = as<Rcpp::S4>(tempS4.slot("item_list"));
      temp_item_list = as<Rcpp::List>(tempS4.slot("item_list"));
      for (int j = 0; j < temp_item_list.size(); j++) {
        tempS4 = as<Rcpp::S4>(temp_item_list[j]);
        output[counter] = as<std::string>(tempS4.slot("item_id"));
        counter++;
      }      
    } else {
      output[counter] = as<std::string>(tempS4.slot("item_id"));
      counter++;
    }
  }
  return(output);
}


//#############################################################################@
//########################### get_item_ids_itempool_cpp ########################
//#############################################################################@
//' Extract testlet ID's that correspond to item ids.
//'
//' @description This function extracts testlet id's of item pool. It has the
//'   same size as item_ids and if an item is not belong to a testlet, a 
//'   NA value will be returned for that item. If all of the items are 
//'   standalone items, than a vector of NA's will be returned . 
//' @param ip An \code{\link{Itempool-class}} object.
//' @return A string vector that holds the values of testlet IDs.
//'
//' @noRd
//'
// [[Rcpp::export]]
Rcpp::StringVector get_testlet_ids_itempool_cpp(Rcpp::S4 ip)
{
  Rcpp::S4 tempS4, temp_element;
  Rcpp::List item_list = ip.slot("item_list");
  Rcpp::List temp_item_list;
  Rcpp::IntegerVector ip_size = get_itempool_size(ip);
  // int n_item = ip_size[2];
  int n_elements = item_list.size();
  //Rcpp::StringVector output(n_item, NA_STRING);
  Rcpp::StringVector output;
  int counter = 0;
  for (int i = 0; i < n_elements; i++)
  {
    temp_element = as<Rcpp::S4>(item_list[i]);
    if (temp_element.inherits("Testlet")) {      
      tempS4 = as<Rcpp::S4>(temp_element.slot("item_list"));
      temp_item_list = as<Rcpp::List>(tempS4.slot("item_list"));
      for (int j = 0; j < temp_item_list.size(); j++) {     
        tempS4 = as<Rcpp::S4>(temp_item_list[j]);
        output.push_back(as<std::string>(temp_element.slot("testlet_id")), 
                         as<std::string>(tempS4.slot("item_id")));  
        //output[counter] = as<std::string>(temp_element.slot("testlet_id"));
        counter++;
      }      
    } else {
      output.push_back(NA_STRING, 
                       as<std::string>(temp_element.slot("item_id"))); 
      counter++;
    }
  }
  return(output);
}



//#############################################################################@
//########################### get_slot_itempool_cpp ############################
//#############################################################################@
//' Extract a string slot of an \code{\link{Itempool-class}} object.
//'
//' @description This function extracts the slot all \code{\link{Item-class}}
//'   objects within an \code{\link{Itempool-class}} object. Note that slot
//'   should hold a character class value.
//' @param ip An \code{\link{Itempool-class}} object.
//' @param slotName A string value of the name of the slot.
//' @return A string vector that holds the values of extracted slot.
//'
//' @noRd
//'
// [[Rcpp::export]]
Rcpp::Nullable<Rcpp::StringVector> get_slot_itempool_cpp(Rcpp::S4 ip,
                                                         std::string slotName)
{
  // This function gets slots

  // I used following two websites for this function:
  // http://gallery.rcpp.org/articles/strings_with_rcpp/
  // http://adv-r.had.co.nz/Rcpp.html#rcpp-classes
  // https://stackoverflow.com/a/25254680/2275286
  if (slotName == "id") return(get_ids_itempool_cpp(ip));
  Rcpp::S4 tempS4;
  Rcpp::List item_list = ip.slot("item_list");
  int noItem = item_list.size();
  Rcpp::StringVector output(noItem);
  int count_empty_str = 0;
  for (int i = 0; i < noItem; i++)
  {
    tempS4 = as<Rcpp::S4> (item_list(i));
    if (Rf_isNull(tempS4.slot(slotName))) {
      output[i] = StringVector::get_na();
      count_empty_str++;
    } else output[i] = as<std::string>(tempS4.slot(slotName));
  }
  if (count_empty_str == noItem)
  {
    return R_NilValue;
  }
  return(output);
}



//#############################################################################@
//########################### get_parameters_itempool_cpp ######################
//#############################################################################@
// [[Rcpp::export]]
Rcpp::NumericMatrix get_parameters_itempool_cpp(Rcpp::S4 ip)
{
  // I used following two websites for this function:
  // http://gallery.rcpp.org/articles/strings_with_rcpp/
  // http://adv-r.had.co.nz/Rcpp.html#rcpp-classes
  // https://stackoverflow.com/a/25254680/2275286
  Rcpp::S4 tempS4;
  Rcpp::List item_list = as<List>(ip.slot("item_list"));
  Rcpp::List parList;

  parList = as<List>(as<Rcpp::S4>(item_list(0)).slot("parameters"));

  int noItem = item_list.size();
  int noPar = parList.size();
  Rcpp::NumericMatrix output(noItem, noPar);
  Rcpp::CharacterVector colNames(noPar), rowNames(noItem) ;

  // Get Column and Row names
  colNames = parList.names();
  rowNames = get_ids_itempool_cpp(ip);
  for (int i = 0; i < noItem; i++)
  {
    tempS4 = as<Rcpp::S4> (item_list(i));
    for (int j = 0; j < noPar; j++)
    {
      parList = as<List>(tempS4.slot("parameters"));
      output(i,j) = as<double>(parList(j));
    }
  }
  output.attr("dimnames") = Rcpp::List::create(rowNames, colNames);
  return(output);
}


//#############################################################################@
//########################### subset_itempool_cpp ##############################
//#############################################################################@
// [[Rcpp::export]]
Rcpp::S4 subset_itempool_cpp(Rcpp::S4 ip, Rcpp::List args)
{
  Rcpp::S4 ipBackup = clone(ip);
  Rcpp::List item_list = as<List>(ipBackup.slot("item_list"));
  Rcpp::S4 item;
  //Rcpp::S4 output = clone(ip);
  int noItem = item_list.size();
  int noArgs = args.size();
  std::vector< std::string > argNames = args.attr("names");

  for (int i = noItem - 1; i >= 0; i--)
  {
    for (int j = 0; j < noArgs; j++)
    {
      item = as<Rcpp::S4>(as<List>(ipBackup.slot("item_list"))[i]);
      if (!Rf_isNull(item.slot(argNames[j])))
      {
        if (as<std::string>(item.slot(argNames[j])) != as<std::string>(args[j]))
          {
            item_list = item_list[ (seq_len(item_list.length()) - 1) != i];
            break;
          }
      }
    }
  }
  if (item_list.length() != 0)
  {
    ipBackup.slot("item_list") = item_list;
  } else stop("There are no elements fitting the criteria. ");
  return ipBackup;
}

//#############################################################################@
//########################### flatten_itempool_cpp #############################
//#############################################################################@
//'
//' This function returns a list of items within item pool. If there is are
//' testlets, the items within each testlet will be extracted and added to
//' the list.
//'
//' @param ip an "Itempool" class object
//' @noRd
// [[Rcpp::export]]
Rcpp::List flatten_itempool_cpp(Rcpp::S4 ip) {
  if (!ip.inherits("Itempool"))
    stop("Please provide a valid 'Itempool' object.");
  Rcpp::List item_list = as<List>(ip.slot("item_list"));
  Rcpp::List testlet_item_list; // List that will hold the items of a testlet
  Rcpp::S4 item; // a testlet or an item element
  Rcpp::IntegerVector ip_size = get_itempool_size(ip);
  unsigned int num_of_sa_items = ip_size["items"]; // Number of standalone items
  Rcpp::StringVector id_list(num_of_sa_items);

  Rcpp::List output(num_of_sa_items);
  int item_index = 0;
  for (int i = 0; i < ip_size["elements"]; i++) {
    item = as<Rcpp::S4>(item_list(i));
    if (item.inherits("Testlet")) {
      testlet_item_list = as<List>(as<S4>(
        item.slot("item_list")).slot("item_list"));
      for (int j = 0; j < testlet_item_list.size(); j++) {
        item = as<Rcpp::S4>(testlet_item_list(j));
        output[item_index] = item;
        id_list[item_index] = as<std::string>(item.slot("item_id"));
        item_index += 1;
      }
    // Here it is assumed that all elements of ip is either "Testlet" or "Item"
    // It is better the implement a line like the following
    // } else if (item.inherits("Item")) {
    } else {
      output[item_index] = item;
      id_list[item_index] = as<std::string>(item.slot("item_id"));
      item_index += 1;
    }
  }
  output.names() = id_list;
  return output;
}

// //##########################################################################@
// //########################### get_content_fast_cpp ##########################
// //##########################################################################@
// // [[Rcpp::export]]
// Rcpp::LogicalVector get_content_fast_cpp(Rcpp::S4 ip, std::string content)
// {
//   Rcpp::S4 ipBackup = clone(ip);
//   Rcpp::List item_list = as<List>(ipBackup.slot("item_list"));
//   Rcpp::S4 item;
//   int noItem = item_list.size();
//   LogicalVector indices(noItem);
//
//   for (int i = noItem - 1; i >= 0; i--)
//   {
//     item = as<Rcpp::S4>(as<List>(ipBackup.slot("item_list"))[i]);
//     if (as<std::string>(item.slot("content")) == content)
//     {
//       indices[i] = true;
//       //item_list = item_list[ (seq_len(item_list.length()) - 1) != i];
//     } else indices[i] = false;
//   }
//   return indices;
// }

//##############################################################################
//##############################################################################
//########################### get_maximum_possible_score #######################
//##############################################################################
//##############################################################################

//##############################################################################
//########################### get_max_possible_score_item_cpp ##################
//##############################################################################
//' This function returns the maximum possible score of an item. It returns an
//' integer.
//' @param item an "Item" class object
//' @noRd
// [[Rcpp::export]]
int get_max_possible_score_item_cpp(Rcpp::S4 item) {
  std::string model = as<std::string>(item.attr("class"));
  // Check polytomous items
  if (model == "GRM" || model == "GPCM" || model == "PCM") {
    return(as<Rcpp::NumericVector>(item.slot("b")).size());
  } else if (model == "GPCM2") {
    return(as<Rcpp::NumericVector>(item.slot("d")).size());
  }
  // Any model other than GRM or GPCM is assumed to be dichotomous.
  return 1;
}

//##############################################################################
//########################### get_max_possible_score_itempool_cpp ##############
//##############################################################################
//' This function returns the maximum possible score of each item in an item
//' pool. It returns a vector of integer values. If there are testlets,
//' the maximum scores of items within testlets will be returned.
//'
//'
//' @param ip an "Itempool" class object
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericVector get_max_possible_score_itempool_cpp(Rcpp::S4 ip) {
  // Items/Testlets of the item pool as individual
  Rcpp::List item_list = flatten_itempool_cpp(ip);
  // Number of standalone items
  unsigned int num_of_sa_items = item_list.size();
  Rcpp::NumericVector output(num_of_sa_items);
  for (unsigned int i = 0; i < num_of_sa_items; i++) {
    output[i] = get_max_possible_score_item_cpp(item_list[i]);
  }
  output.names() = item_list.names();
  return output;
}


