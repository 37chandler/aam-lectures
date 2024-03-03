import os

os.chdir('/users/chandler/dropbox/teaching/aam/presenting-models/')

results = []
partial_line = []

with open("backpain-raw.txt",'r') as infile :
    for line in infile :
        line = line.strip().strip(".").strip(".&")
        line = line.split()
        if not partial_line :
            partial_line = line
        else :
            partial_line.extend(line)
            results.append(partial_line)
            partial_line = []

print("read")

# Headers
headers = """
patient_number
back_pain_severity
month_of_pregnancy_pain_started
age_of_patient_years
height_of_patient_metres
weight_of_patient_start_kg
weight_of_patient_end_kg
weight_of_baby_kg
number_of_children_previous_pregnancies
backache_previous_pregnancy
factor_tablets
factor_hot_water_bottle
factor_hot_bath
factor_cushion_behind_back
factor_standing
factor_sitting
factor_lying
factor_walking
factor_fatigue
factor_bending
factor_lifting
factor_making_beds
factor_washing_up
factor_ironing
factor_bowel_action
factor_intercourse
factor_coughing
factor_sneezing
factor_turning_in_bed
factor_standing_again
factor_sitting_again
factor_lying_again
factor_walking_again
""".split()

with open("backpain.txt",'w') as ofile :
    ofile.write("\t".join(headers) + "\n")
    for line in results :
        ofile.write("\t".join(line) + "\n")

print("done")