CREATE TABLE FL_insurance_sample("policyID" INTEGER, "statecode" CHAR, "county" CHAR, "eq_site_limit" DECIMAL, "hu_site_limit" DECIMAL, "fl_site_limit" DECIMAL, "fr_site_limit" DECIMAL, "tiv_2011" DECIMAL, "tiv_2012" DECIMAL, "eq_site_deductible" DECIMAL, "hu_site_deductible" DECIMAL, "fl_site_deductible" DECIMAL, "fr_site_deductible" DECIMAL, "point_latitude" DECIMAL, "point_longitude" DECIMAL, "line" CHAR, "construction" CHAR, "point_granularity" INTEGER);
.mode csv
.import ~/DScourseS19/ProblemSets/PS3/FL_insurance_sample.csv FL_insurance_sample;
SELECT * FROM FL_insurance_sample LIMIT 10;
SELECT DISTINCT county FROM FL_insurance_sample;
SELECT AVG(tiv_2012-tiv_2011) FROM FL_insurance_sample;
SELECT construction, COUNT(*) FROM FL_insurance_sample GROUP BY construction;
