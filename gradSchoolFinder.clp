;Defining the templates 
(deftemplate student (slot studName) (slot studAge) (slot studMajor) (slot studCountry))
(deftemplate gre(slot greScore))
(deftemplate toefl (slot isValidToefl))
(deftemplate cumulative (slot cumulativeValue))
(deftemplate workexperience(slot workExperience))
(deftemplate gpaTemp(slot gpaValue))
;Defining the global variables
(defglobal 
    ?*studentName* = nil
    ?*age* = nil
    ?*major* = nil
    ?*country* = nil
    ?*quant* = nil
    ?*verbal* = nil
    ?*awa* = nil
    ?*reading* = nil
    ?*writing* = nil
    ?*speaking* = nil
    ?*listening* = nil
	?*gpaType* = nil
    ?*gpa* = nil
    ?*workExperience* = nil
    ?*sopValue* = nil
	?*lorValue* = nil
	?*communityService* = nil
    )
;Rule for taking the input from the command line interface
(defrule takeInput
    =>
    (printout t "Please enter your full name: " crlf)
    (bind ?*studentName* (read t))
    (printout t "Please enter your age: " crlf)
    (bind ?*age* (read t))
    (printout t "Please enter your major" crlf)
    (bind ?*major* (read t))
    (printout t "Please enter your country" crlf)
    (bind ?*country* (read t))
	(printout t "Please enter your GRE quant score (130-170):" crlf)
    (bind ?*quant* (read t))
	(printout t "Please enter your GRE verbal score (130-170):" crlf)
    (bind ?*verbal* (read t))
	(printout t "Please enter your GRE awa score(1-6)" crlf)
    (bind ?*awa* (read t))
	(printout t "Please enter your TOEFL reading score (1-30):" crlf)
    (bind ?*reading* (read t))
	(printout t "Please enter your TOEFL writing score (1-30):" crlf)
    (bind ?*writing* (read t))
	(printout t "Please enter your TOEFL speaking score (1-30):" crlf)
    (bind ?*speaking* (read t))
	(printout t "Please enter your TOEFL listening score (1-30):" crlf)
    (bind ?*listening* (read t))
    (printout t "Please enter your GPA type: 4 or 10" crlf)
    (bind ?*gpaType* (read t))
	(printout t "Please enter your GPA: " crlf)
    (bind ?*gpa* (read t))
	(printout t "Please enter your Work Experience in years:" crlf)
    (bind ?*workExperience* (read t))
	(printout t "Please enter your LOR value(1-10):" crlf)
    (bind ?*lorValue* (read t))
	(printout t "Please enter your SOP value(1-10):" crlf)
    (bind ?*sopValue* (read t))
	(printout t "Please enter your Community Service value(1-10)" crlf)
    (bind ?*communityService* (read t))
	
	(assert (student (studName ?*studentName*)(studAge ?*age*)(studMajor ?*major*)(studCountry ?*country*)))
	(assert (workexperience(workExperience ?*workExperience*)))
	
;Function calls to those functions that compute important parameters for recommendation
	(GPAConversion ?*gpaType* ?*gpa*)
	(TOEFL ?*reading* ?*writing* ?*speaking* ?*listening*)
	(GRE ?*quant* ?*verbal* ?*awa*)
	(CumulativeScore ?*sopValue* ?*lorValue* ?*communityService*)
    )
;Functions that are helpful for computations
(deffunction GPAConversion(?gpaType ?gpa)
    (if(eq ?gpaType 4) then
;	(printout t "GPAType is 4, GPA is :" ?gpa crlf)
	(assert (gpaTemp (gpaValue ?*gpa*)))
	else
;		(printout t "GPA is:" (/ (* ?gpa 4) ?gpaType) crlf)
		(assert (gpaTemp (gpaValue (/ (* ?gpa 4) ?gpaType))))
		)
    )	
(deffunction TOEFL(?reading ?writing ?speaking ?listening)
	(bind ?toeflScore (+ (+ (+ ?reading ?writing) ?speaking) ?writing))
	(if(< ?toeflScore 80) then
;		(printout t "TOEFL is very less:" ?toeflScore crlf)
	    (assert (toefl(isValidToefl 0)))
	else
;		(printout t "TOEFL is valid" ?toeflScore crlf)
	    (assert (toefl (isValidToefl 1))))
	)

(deffunction GRE(?quant ?verbal ?awa)
	(bind ?tempGRE (+ (+ ?quant ?verbal)?awa))
	(assert (gre(greScore ?tempGRE)))
	)

(deffunction CumulativeScore(?sopValue ?lorValue ?communityService)
	(bind ?tempCumulative (+ (+ ?sopValue ?lorValue)?communityService))
	(assert (cumulative(cumulativeValue ?tempCumulative)))
	)
;Rules for verifying the student's validity 
(defrule invalidStudentRule
	(declare (salience 300))
	(or (and (toefl{isValidToefl == 0}) (gre{greScore < 263}) (student{studAge < 18})) (and (toefl{isValidToefl == 0}) (gre{greScore < 263})) (and (toefl{isValidToefl == 0})(student{studAge < 18})) (and (gre{greScore < 263}) (student{studAge < 18})) )
    =>
    (printout t "You DO NOT meet the eligibility criteria because of the following reasons: " crlf)
     )

(defrule validStudentRule
	(declare (salience 300))
	(toefl{isValidToefl == 1})(gre{greScore >= 263})(student{studAge >= 18})
    =>
    (printout t "Please find the following suggestions: " crlf)
	(printout t "************************************************** " crlf)
     )

(defrule ageRule
	(declare (salience 200))
    (student{studAge < 18})
    =>
	(printout t "*******************" crlf)
    (printout t "Your age does not fit the graduate admissions criteria. Please apply after you turn 18." crlf)
     )
(defrule toeflRule
	(declare (salience 100))
    (toefl{isValidToefl == 0})
    =>
	(printout t "*******************" crlf)
    (printout t "Low TOEFL Scores. Please retake before you apply to any of the schools in Illinois." crlf)
     )
	 
(defrule greRule
	(declare (salience 90))
    (gre{greScore < 263})
    =>
	(printout t "*******************" crlf)
    (printout t "Low GRE Scores. You will be denied admission in any graduate schools in Illinois." crlf)
     )
;Rules for suggesting universities to student according to his skills
(defrule topNotchRule1
    (toefl{isValidToefl == 1})(MAIN::gre{greScore > 320})(MAIN::cumulative{cumulativeValue > 25})(workexperience{workExperience > 0})(gpaTemp{gpaValue > 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Northern Illinois University, DePaul" crlf)
	(printout t "Moderate: University of Illinois-Chicago, Northwestern University" crlf)
	(printout t "Ambitious: University of Chicago, University of Illinois at Urbana Champaign" crlf)
     )
(defrule topNotchRule2
    (toefl{isValidToefl == 1})(MAIN::gre{greScore > 320})(MAIN::cumulative{cumulativeValue > 25})(workexperience{workExperience == 0})(gpaTemp{gpaValue > 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Illinois Institute of Technology, DePaul" crlf)
	(printout t "Moderate: University of Illinois-Chicago, Northern Illinois University" crlf)
	(printout t "Ambitious: University of Chicago, Northwestern University" crlf)
     )
(defrule topNotchRule3
    (toefl{isValidToefl == 1})(MAIN::gre{greScore > 320})(MAIN::cumulative{cumulativeValue <= 25})(workexperience{workExperience > 0})(gpaTemp{gpaValue > 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Illinois Institute of Technology, DePaul" crlf)
	(printout t "Moderate: University of Illinois-Chicago, Northern Illinois University" crlf)
	(printout t "Ambitious: University of Chicago, Northwestern University" crlf)
     )
(defrule topNotchRule4
    (toefl{isValidToefl == 1})(MAIN::gre{greScore > 320})(MAIN::cumulative{cumulativeValue > 25})(workexperience{workExperience > 0})(gpaTemp{gpaValue <= 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Lewis University, DePaul" crlf)
	(printout t "Moderate: Elmhurst College, Illinois Institute of Technology" crlf)
	(printout t "Ambitious: University of Illinois-Chicago, Northwestern University" crlf)
     )
(defrule topNotchRule5
    (toefl{isValidToefl == 1})(MAIN::gre{greScore > 320})(MAIN::cumulative{cumulativeValue <= 25})(workexperience{workExperience > 0})(gpaTemp{gpaValue <= 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Lewis University, Illinois Institute of Technology" crlf)
	(printout t "Moderate: University of Illinois-Chicago, Knox College" crlf)
	(printout t "Ambitious: University of Chicago, University of Illinois at Urbana Champaign" crlf)
     )
(defrule topNotchRule6
    (toefl{isValidToefl == 1})(MAIN::gre{greScore > 320})(MAIN::cumulative{cumulativeValue <= 25})(workexperience{workExperience == 0})(gpaTemp{gpaValue > 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Devry University, DePaul University" crlf)
	(printout t "Moderate: University of Illinois-Springfield, Knox College" crlf)
	(printout t "Ambitious: University of Illinois-Chicago, Illinois State University" crlf)
     )
(defrule topNotchRule7
    (toefl{isValidToefl == 1})(MAIN::gre{greScore > 320})(MAIN::cumulative{cumulativeValue > 25})(workexperience{workExperience == 0})(gpaTemp{gpaValue <= 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Devry University, Lewis University" crlf)
	(printout t "Moderate: University of Illinois-Chicago, Knox College" crlf)
	(printout t "Ambitious: University of Chicago, Illinois State University" crlf)
     )
(defrule topNotchRule8
    (toefl{isValidToefl == 1})(MAIN::gre{greScore > 320})(MAIN::cumulative{cumulativeValue <= 25})(workexperience{workExperience == 0})(gpaTemp{gpaValue <= 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Lake Forest University, Bradley University" crlf)
	(printout t "Moderate: Devry University, Knox College" crlf)
	(printout t "Ambitious: University of Illinoi-Chicago, Illinois State University" crlf)
     ) 
(defrule SecondLevelRule1
    (toefl{isValidToefl == 1})(MAIN::gre{greScore > 300 && greScore <= 320})(MAIN::cumulative{cumulativeValue > 25})(workexperience{workExperience > 0})(gpaTemp{gpaValue > 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Lake Forest University, Bradley University" crlf)
	(printout t "Moderate: Northern Illinois University, Illinois State University" crlf)
	(printout t "Ambitious: University of Illinoi-Chicago, University of Chicago" crlf)
     ) 
(defrule SecondLevelRule2
    (toefl{isValidToefl == 1})(MAIN::gre{greScore > 300 && greScore <= 320})(MAIN::cumulative{cumulativeValue > 25})(workexperience{workExperience == 0})(gpaTemp{gpaValue > 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Lake Forest University, Lewis University" crlf)
	(printout t "Moderate: Northern Illinois University, Devry University" crlf)
	(printout t "Ambitious: Illinois State University, Illinois Institute of Technology"crlf)
     )
(defrule SecondLevelRule3
    (toefl{isValidToefl == 1})(MAIN::gre{greScore > 300 && greScore <= 320})(MAIN::cumulative{cumulativeValue <= 25})(workexperience{workExperience > 0})(gpaTemp{gpaValue > 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Devry University, DePaul University" crlf)
	(printout t "Moderate: Northern Illinois University, Illinois State University" crlf)
	(printout t "Ambitious: University of Illinoi-Chicago, Illinois Institute of Technology" crlf)
     ) 
(defrule SecondLevelRule4
    (toefl{isValidToefl == 1})(MAIN::gre{greScore > 300 && greScore <= 320})(MAIN::cumulative{cumulativeValue > 25})(workexperience{workExperience > 0})(gpaTemp{gpaValue <= 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Elmhurst College, University of Illinois-Springfield" crlf)
	(printout t "Moderate: Lewis University, Illinois State University" crlf)
	(printout t "Ambitious: University of Illinoi-Chicago, Illinois Institute of Technology" crlf)
     ) 
(defrule SecondLevelRule5
    (toefl{isValidToefl == 1})(MAIN::gre{greScore > 300 && greScore <= 320})(MAIN::cumulative{cumulativeValue <= 25})(workexperience{workExperience > 0})(gpaTemp{gpaValue <= 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Elmhurst College, University of Illinois-Springfield" crlf)
	(printout t "Moderate: Illinois Institute of Technology, Devry University" crlf)
	(printout t "Ambitious: Illinois State University, Lewis University" crlf)
     ) 
(defrule SecondLevelRule6
    (toefl{isValidToefl == 1})(MAIN::gre{greScore > 300 && greScore <= 320})(MAIN::cumulative{cumulativeValue <= 25})(workexperience{workExperience == 0})(gpaTemp{gpaValue > 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Elmhurst College, DePaul University" crlf)
	(printout t "Moderate: Knox College, Devry University" crlf)
	(printout t "Ambitious: Illinois Institute of Technology, University of Illinoi-Chicago" crlf)
     ) 
(defrule SecondLevelRule7
    (toefl{isValidToefl == 1})(MAIN::gre{greScore > 300 && greScore <= 320})(MAIN::cumulative{cumulativeValue > 25})(workexperience{workExperience == 0})(gpaTemp{gpaValue <= 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Elmhurst College, University of Illinois-Springfield"crlf)
	(printout t "Moderate: Knox College, University of Illinoi-Chicago" crlf)
	(printout t "Ambitious: Illinois Institute of Technology, University of Illinoi-Urbana Champaign" crlf)
     )
(defrule SecondLevelRule8
    (toefl{isValidToefl == 1})(MAIN::gre{greScore > 300 && greScore <= 320})(MAIN::cumulative{cumulativeValue <= 25})(workexperience{workExperience == 0})(gpaTemp{gpaValue <= 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Dominican University, Bradley University"crlf)
	(printout t "Moderate: Knox College, University of Illinois-Springfield" crlf)
	(printout t "Ambitious: Illinois Institute of Technology, University of Illinoi-Chicago" crlf)
     )
(defrule ThirdLevelRule1
    (toefl{isValidToefl == 1})(MAIN::gre{greScore >= 263 && greScore <= 300})(MAIN::cumulative{cumulativeValue > 25})(workexperience{workExperience > 0})(gpaTemp{gpaValue > 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Augustana University, Bradley University"crlf)
	(printout t "Moderate: Illinois State University, University of Illinois-Springfield" crlf)
	(printout t "Ambitious: Illinois Institute of Technology, University of Illinoi-Chicago" crlf)
	)
(defrule ThirdLevelRule2
    (toefl{isValidToefl == 1})(MAIN::gre{greScore >= 263 && greScore <= 300})(MAIN::cumulative{cumulativeValue > 25})(workexperience{workExperience == 0})(gpaTemp{gpaValue > 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Augustana University, Lewis University"crlf)
	(printout t "Moderate: Illinois State University, Dominican University" crlf)
	(printout t "Ambitious: Illinois Institute of Technology, Illinois State University" crlf)
	)
(defrule ThirdLevelRule3
    (toefl{isValidToefl == 1})(MAIN::gre{greScore >= 263 && greScore <= 300})(MAIN::cumulative{cumulativeValue <= 25})(workexperience{workExperience > 0})(gpaTemp{gpaValue > 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Devry University, Lewis University"crlf)
	(printout t "Moderate: Illinois State University, Lake Forest College" crlf)
	(printout t "Ambitious: Illinois Institute of Technology, Bradley University " crlf)
	)
(defrule ThirdLevelRule4
    (toefl{isValidToefl == 1})(MAIN::gre{greScore >= 263 && greScore <= 300})(MAIN::cumulative{cumulativeValue > 25})(workexperience{workExperience > 0})(gpaTemp{gpaValue <= 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Principia College, Eastern Illinois University"crlf)
	(printout t "Moderate: Illinois College, Lake Forest College" crlf)
	(printout t "Ambitious: Illinois Institute of Technology, Illinois State University " crlf)
	)
(defrule ThirdLevelRule5
    (toefl{isValidToefl == 1})(MAIN::gre{greScore >= 263 && greScore <= 300})(MAIN::cumulative{cumulativeValue <= 25})(workexperience{workExperience > 0})(gpaTemp{gpaValue <= 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Konox College, Millikin University"crlf)
	(printout t "Moderate: Illinois College, Lake Forest College" crlf)
	(printout t "Ambitious: Illinois Institute of Technology, Bradley University " crlf)
	)
(defrule ThirdLevelRule6
    (toefl{isValidToefl == 1})(MAIN::gre{greScore >= 263 && greScore <= 300})(MAIN::cumulative{cumulativeValue <= 25})(workexperience{workExperience == 0})(gpaTemp{gpaValue > 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Illinois College, Eastern Illinois University"crlf)
	(printout t "Moderate: Knox College, Illinois Wesleyan University" crlf)
	(printout t "Ambitious: Wheaton College, Augustana University " crlf)
	)
(defrule ThirdLevelRule7
    (toefl{isValidToefl == 1})(MAIN::gre{greScore >= 263 && greScore <= 300})(MAIN::cumulative{cumulativeValue > 25})(workexperience{workExperience == 0})(gpaTemp{gpaValue <= 3})(student{studAge >= 18})
    =>
    (printout t "Safe: Robert Morris University Illinois, Millikin University"crlf)
	(printout t "Moderate: Knox College, Illinois Wesleyan University" crlf)
	(printout t "Ambitious: Illinois College, Augustana University " crlf)
	)
(defrule ThirdLevelRule8
    (toefl{isValidToefl == 1})(MAIN::gre{greScore >= 263 && greScore <= 300})(MAIN::cumulative{cumulativeValue <= 25})(workexperience{workExperience == 0})(gpaTemp{gpaValue <= 3})(student{studAge >= 18})
    =>
    (printout t "Safe: McKendree University, North Park University"crlf)
	(printout t "Moderate: Robert Morris University Illinois,  Millikin University" crlf)
	(printout t "Ambitious: Knox College, Augustana University " crlf)
	)

(defrule workExperienceRule
	(declare (salience 100))
    (workexperience{workExperience > 0})(toefl{isValidToefl == 1})(student{studAge >= 18})
    =>
    (printout t " " crlf)
	(printout t "Your work experience is very much useful in the admission process. Make sure you mention it clearly on your resume and SOP" crlf)
	(printout t " " crlf)
     )
(defrule workExperienceRule2
	(declare (salience 100))
    (workexperience{workExperience == 0})(toefl{isValidToefl == 1})(student{studAge >= 18})
    =>
    (printout t " " crlf)
	(printout t "Please mention your good academic projects or internships in your SOP and Resume. They add weight to your application." crlf)
	(printout t " " crlf)
     )

(reset)
(facts)
(run)