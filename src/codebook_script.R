size_book <- data.frame(
  category <- rep("Size", 13),
  var_name <- c("school_size",
                "dorm_capacity",
                "total_enrolled", 
                "male_enrolled",
                "female_enrolled",
                "native_enrolled",
                "asian_enrolled",
                "black_enrolled", 
                "hispanic_enrolled", 
                "pacific_enrolled", 
                "white_enrolled", 
                "mixed_enrolled", 
                "first_years_enrolled"),
  
  description <- c(
"Ordinal variable measuring student enrollment: 1 = Under 1,000; 2 = 1,000 - 4,999; 3 = 5,000 - 9,999; 4 = 10,000 - 19,999; 5 = 20,000 and above",
"Total dorm capacity",
"Total number of students enrolled",
"Total number of male students enrolled",
"Total number of female students enrolled",
"Total number of students enrolled identifying as native american",
"Total number of students enrolled identifying as asian",
"Total number of students enrolled identifying as black",
"Total number of students enrolled identifying as hispanic",
"Total number of students enrolled identifying as a pacific islander",
"Total number of students enrolled identifying as white",
"Total number of students enrolled identifying as mixed",
"Total number of first-year students enrolled"
  )
)



cost_book <- data.frame(
  category <- rep("Cost", 5),
  var_name <- c("room_board", 
                "pct_fin_aid", 
                "avg_fin_aid", 
                "in_state_tuition", 
                "out_state_tuition"),
  description <- c(
    "Combined charge for room and board",
    "Percentage of undergraduate students awarded federal, state, local, institutional or other sources of grant aid. Ranges from 0-100",
    "Average amount of federal, state, local, institutional or other sources of grant aid granded to undergraduate students. Ranges from 250-53290",
    "Average in-state tuition for full-time undergraduate students",
    "Average out-of-state tuition for full-time undergraduate students"
  )
)

exclusivity_book <- data.frame(
  category <- rep("Exclusivity", 3),
  var_name <- c("act_25", 
                "act_75", 
                "prop_admit"),
  description <- c("ACT composite 25th perentile score. Schools that submitted SAT scores had those entries converted to the ACT equivalent",
                   "ACT composite 25th perentile score. Schools that submitted SAT scores had those entries converted to the ACT equivalent",
                   "Proportion of admitted students from applied students"
  )
)

mobility_book <- data.frame(
  category <- rep("Mobility", 6),
  var_name <- c("kq5_cond_parq1", 
                "ktop1pc_cond_parq1", 
                "mr_kq5_pq1", 
                "mr_ktop1_pq1", 
                "trend_parq1", 
                "trend_bottom40"),
  description <- c(
    "Percent of children who reach the Top 20% of the income distribution among children with parents in the Bottom 20% of the income distribution",
    "Percent of children who reach the Top 1% of the income distribution among children with parents in the Bottom 20% of the income distribution",
    "Mobility Rate: Percent of students who have parents in the Bottom 20% of the income distribution and reach the Top 20% of the income distribution",
    "Upper-Tail Mobility Rate: Percent of students who have parents in the Bottom 20% of the income distribution and reach the Top 1% of the income distribution",
    "Change in % of Parents from the Bottom 20% of the income distribution between the 1980 and 1991 cohorts",
    "Change in % of Parents from the Bottom 40% of the income distribution between the 1980 and 1991 cohorts"
  )
)

diversity_book <- data.frame(
  category <- rep("Diversity", 9),
  var_name <- c("prop_male", 
                "prop_female", 
                "prop_native", 
                "prop_black", 
                "prop_white",
                "prop_mixed", 
                "prop_asian", 
                "prop_hispanic", 
                "prop_pacific"),
  description <- c(
    "Proportion of students identifying as male",
    "Proportion of students identifying as female",
    "Proportion of students identifying as native american",
    "Proportion of students identifying as black",
    "Proportion of students identifying as white",
    "Proportion of students identifying as mixed",
    "Proportion of students identifying as asian",
    "Proportion of students identifying as hispanic",
    "Proportion of students identifying as a pacific islander"
  )
)

EE_book <- data.frame(
  category <- rep("Educational Experience", 8),
  var_name <- c("highest_degree", 
                "student_faculty_ratio", 
                "prop_prof", 
                "prop_instr", 
                "control", 
                "part_time_grad", 
                "part_time_undergrad", 
                "retention_rate"),
  description <- c(
    "Highest level of degree offered by the college: NA = Not Available; 1 = Postsecondary award, certificate or diploma of less than one academic year; 2 = Postsecondary award, certificate or diploma of at least one but less than two academic years; 3 = Associate's degree ; 4 = Postsecondary award, certificate or diploma of at least two but less than four academic years; 5 = Bachelor's degree; 6 = Postbaccalaureate certificate; 7 = Master's degree; 8 = Post-master's certificate; 9 = Doctor's degree", 
    "Student:faculty ratio. Ranges from 1 to 142", 
    "proportion of professors to undergraduate students", 
    "Proportion of instructors to undergraduate students", 
    "A categorical variable indicating if the institution is public, private not-for-profit, or private-for-profit", 
    "Indicator variable for if institution enrolls part-time graduate students", 
    "Indicator variable for if institution enrolls part-time undergraduate students.", 
    "Full-time retention rate. Ranges from 0-100."
  )
)

colnames(size_book) <- c("Category", "Variable", "Description")
colnames(cost_book) <- c("Category", "Variable", "Description")
colnames(exclusivity_book) <- c("Category", "Variable", "Description")
colnames(mobility_book) <- c("Category", "Variable", "Description")
colnames(diversity_book) <- c("Category", "Variable", "Description")
colnames(EE_book) <- c("Category", "Variable", "Description")

shiny_codebook <- do.call("rbind", list(size_book,
                                  cost_book,
                                  exclusivity_book,
                                  mobility_book,
                                  diversity_book,
                                  EE_book))

save(shiny_codebook, file = "shiny_codebook.Rda")
