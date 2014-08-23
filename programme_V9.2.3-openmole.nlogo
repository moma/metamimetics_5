;; TO DO
;; voir avec les mini le problème de l'assymétrie :OK
;; faire la distinction entre gains subjectifs et objectifs : OK
;; variables globales ?
;; score à moyenner : fait
globals [
  
cooperation-rate 
satisfaction-rate
fraction-best
maxi
mini
conf
anti
maxi-auto
mini-auto 
conf-auto
anti-auto
life-distribution
cumulative-census-dist
theta1-mean-maxi
theta1-mean-mini
theta1-mean-conf
theta1-mean-anti
theta2-mean-maxi
theta2-mean-mini
theta2-mean-conf
theta2-mean-anti
ratio-maxi
ratio-mini
ratio-conf
ratio-anti
satisfaction-mean-maxi
satisfaction-mean-mini
satisfaction-mean-conf
satisfaction-mean-anti
mean-age-auto-conf
mean-age-auto-anti
mean-age-auto-maxi
mean-age-auto-mini
mean-age-autre-maxi
mean-age-autre-mini
mean-age-autre-conf
mean-age-autre-anti
progres
n-progres
density
inicoop


;Initial-prob-update-behavior
;Initial-prob-update-rule
;Initial-weighting-history     
]

;; variables relatives aux tortues
turtles-own [
    ;description of agent's strategies
    cooperate?       ;; patch will cooperate
    rule                   ;; patch will have one of four rules: 1=Maxi 2=mini 3=conformist 4=anticonformist      
    likelihood-to-move ;; proba de chercher à bouger
    theta_1 ;; prob de changer son comportement
    theta_2 ;; prob de changer sa règle
    auto ;; propensity to be auto-biographic
    weighting-history ;; poids associé au score     
    ;agent decision
    rule?   ;; tortue à une règle qu’elle veut changer ou non
    behavior?  ;; tortue à un comportement qu’elle veut changerou non
    move?   ;; tortue est à un endroit d’ou elle veut bouger ou non
    

    ;agents state
    score                ;; objective score resulting from interaction of neighboring patches. It is dictated by the PD payoffs and the discount factor
    score2 ;; subjective score resulting from interaction of neighboring patches. It is dictated by the PD payoffs and the discount factor
    last-score ;; score pondéré à t-1
    last-score2 ;; score2 pondéré à t-1
    last-inst-score ;; score instantané à t-1
    inst-score ;; score instantané à t
    satisfaction ;; score de satisfaction
    age   ;;
 
        
    ;agent's environement
    count-majority-rule ; nbr de tortues voisines ayant la règle majoritaire à t
    count-minority-rule ; nbr de tortues voisines ayant la règle minoritaire à t 
    last-count-majority-rule ; nbr de tortues voisines ayant la règle majoritaire à t-1 
    last-count-minority-rule ; nbr de tortues voisines ayant la règle minoritaire à t-1
       
    t ; variable utilise pour le nombre d'interactions / années
 ]


to common-setup
    ask patches [              ;; dit aux patches
     set pcolor 0    ;; d’avoir la couleur 0
      if random-float 1 < density [ sprout 1 ]  ;; s’il n’y a pas de tortue en créer une
              ]
    ask turtles [
      set rule (random 4) + 1 ;; donne une stratégie à chaque tortue       
      ifelse random-float 1.0 < (inicoop / 100)  ;; si la coopération initiale de la tortue est supérieure à un entier alors ont dit qu’il coopère sinon on dit qu’il ne coopère pas  
        [set cooperate? true]
        [set cooperate? false]
      set likelihood-to-move Initial-like-to-move
      set theta_1 Initial-prob-update-behavior
      set theta_2 Initial-prob-update-rule
      set weighting-history Initial-weighting-history               
      set auto auto_biographic_agents
      ifelse auto         
          [set shape "target"] ;; chaque tortue est contente au départ
          [set shape "face happy"] 
          
      set rule? false  
      set behavior? false  
      set move? false
      
      
      set score 0.0 ;; score objectif de zéro au départ
      set score2 0.0 ;; score subjectif
      set last-score 0.0
      set t 0  ; nombre d'interaction par pas de temps ou "annees"
          ]
    
  ask turtles[establish-color]
        
      init-age-FR2011 ;; utilise la fonction init-age pour déterminer l’age des tortues
      set-life-distribution-FR2011 ;; utilise la fonction set-life-distribution pour déterminer l’espérance de vie des tortues
      set-outputs
      
      reset-ticks

end

to setup
  clear-all  ;; monde vide
        ;agent's variables  
        
        
        ;environmental variables
        set strength-of-dilemma strength-of-dilemma
        set density mydensity
        set inicoop i-coop
        set Transcription-error Transcription-error
        set erreur-inference-regle erreur-inference-regle
        set erreur-inference-comportement erreur-inference-comportement
     common-setup 
end 


to run-to-grid [tmax]

common-setup
setup
while [ticks < tmax]
[go]
reset-ticks

end 


to go
    ask turtles [interact] 
    ask turtles [analyse-neighbors] 
    decision-stage
    learning-stage 
    ask turtles [calculate-satisfaction]
    moving-stage
    
    set-outputs            
    update-plot    
    reset-decisions 
    if generations ;; we have overlapping generations of agents with distributions as FR census.
       [replacement]
    ask turtles [set age age + 1]   
    update-views
    
    ask turtles [
      ifelse auto
      [ifelse am-i-progressing?; or am-i-the-best-global?
        [set shape "target"] ;; si am-i-the-best = true ou am-i-progressing = true ou si am-i-the-best-global = true la tortue a une tête contente
        [set shape "circle"]] ;; si am-i-the-best = false la tortue a une tête pas contente]
      [ifelse am-i-the-best? ; or am-i-the-best-global?
        [set shape "face happy"] ;; si am-i-the-best = true ou am-i-progressing = true ou si am-i-the-best-global = true la tortue a une tête contente
        [set shape "face sad"] ;; si am-i-the-best = false la tortue a une tête pas contente]
      ]]  
  update-views
  tick
end


to-report weigh_score_objective [current_score history_score weight]
      ;report current_score + weight * history_score
      report (1 - weight) * current_score + weight * history_score
end

to-report weigh_score_subjective [current_score history_score weight]
    ifelse rule = 2
    [report (1 - weight) * (1 - current_score) + weight * history_score]
    [report (1 - weight) * current_score + weight * history_score]
      
end

to interact  ;; calculates the agent's payoff for Prisioner's Dilema. Each agents plays only with its neighbors
  let total-cooperators count (turtles-on neighbors) with [cooperate?] ;; calcul nombre coopérateur dans les voisins de chaque tortue
  set last-inst-score inst-score  
  ifelse cooperate? 
    [ifelse count(turtles-on neighbors) > 0 [set inst-score (total-cooperators * ( 1 - strength-of-dilemma)) / count(turtles-on neighbors) ][set inst-score (total-cooperators * ( 1 - strength-of-dilemma))]] ;; cooperator gets score of a multiple of neighbors who cooperated 
    [ifelse count(turtles-on neighbors) > 0 [set inst-score (total-cooperators + (count (turtles-on neighbors) - total-cooperators) * strength-of-dilemma) / count(turtles-on neighbors) ][set inst-score (total-cooperators + (count (turtles-on neighbors) - total-cooperators) * strength-of-dilemma)] ] ;; non-cooperator get score of a multiple of the neighbors who haven’t cooperated
  set last-score score ;; on stocke le score à t-1 dans score 
  set last-score2 score2 ;; on stocke le score subjectif à t-1 dans score2
  set score weigh_score_objective inst-score last-score weighting-history
  set score2 weigh_score_subjective inst-score last-score weighting-history
end


to analyse-neighbors
  
  let mylist [rule] of (turtle-set turtles-on neighbors self)
  set mylist modes mylist  
  set last-count-majority-rule count-majority-rule 
  set count-majority-rule length mylist

  let mylist2 [rule] of (turtle-set turtles-on neighbors self)
  let mylist3 []
  let j 1
  while [empty? mylist3] [
  let i 1
  repeat 4 [
    if length filter [? = i] mylist2 = j  [set mylist3 lput i mylist3] 
    set i i + 1
    ]
  set j j + 1
  ] 
  set last-count-minority-rule count-minority-rule 
  set count-minority-rule length mylist3
 
end

  
  
;; A VERIFIER ;;;;;;;;;;;;;;;;;;
to decision-stage
   ask turtles [
     let i_am_statisfied true
         ifelse ( auto ) 
            [set i_am_statisfied am-i-progressing?]
            [set i_am_statisfied am-i-the-best?]
     let change_behavior false ; if the agent updates its rule, it also updates its behavior
     
     if not i_am_statisfied
     [ 
     ifelse (random-float 1 < likelihood-to-move and not all? neighbors [any? turtles-here])
     [set move? true]
     [
       set move? false       
       if (auto and (age > 12))
       [set auto false
        set i_am_statisfied am-i-the-best? 
        ]
       
       if not i_am_statisfied
       [     
       if (random-float 1 < theta_2)
          [set rule? true
           set change_behavior true]
       if (change_behavior or (random-float 1 < theta_1 and (not i_am_statisfied)))
            [set behavior? true]                   
       
     ]]]
   ; may be this is not necessary with auto ) true  
   if not (auto)
 [  if age < 12  ;; vérifier comment se passe l’apprentissage
        [set rule? false] ]  
   ]
end

to learning-stage
    ask turtles [ 
      if rule?   ;; si on peut changer la règle
      [
            select-rule              
     ]
       if behavior? ;; si on peut changer le comportement
      [select-behavior]
    ]    
end



to calculate-satisfaction
  ifelse auto
  [ifelse am-i-progressing?
    [set satisfaction 1]
    [set satisfaction 0]
    ]
  [
    if rule = 1 ;; si règle maxi
      [
          let top [score] of max-one-of (turtle-set turtles-on neighbors self) [score]
          let bottom [score] of min-one-of (turtle-set turtles-on neighbors self) [score]
          ifelse (top - bottom) = 0
          [set satisfaction 1]
          [set satisfaction (score - bottom) / (top - bottom)] ;;sinon calcul de la satisfaction par rapport au score
        ]
    if rule = 2 ;; si règle mini
      [
          let top [score2] of max-one-of (turtle-set turtles-on neighbors self) [score2]
          let bottom [score2] of min-one-of (turtle-set turtles-on neighbors self) [score2]
          ifelse (top - bottom) = 0
          [set satisfaction 1]
          [set satisfaction (top - score) / (top - bottom)] ;;sinon calcul de la satisfaction par rapport au score
          
          
         ; let top [-1 * score] of min-one-of (turtle-set turtles-on neighbors self) [score]
         ; let bottom [-1 * score] of max-one-of (turtle-set turtles-on neighbors self) [score]
         ; ifelse (top - bottom) = 0
         ; [set satisfaction 1]
         ; [set satisfaction ((-1 * score) - bottom) / (top - bottom)] 
        ]
    if rule = 3 ;; si règle conf
      [
          let top-rule one-of majority-rules
          let top count (turtle-set turtles-on neighbors self) with [rule = top-rule]/ count (turtle-set turtles-on neighbors self) 
          let bottom-rule one-of minority-rules
          let bottom count (turtle-set turtles-on neighbors self) with [rule = bottom-rule]/ count (turtle-set turtles-on neighbors self)     
          let my-rule rule
          let my-group count (turtle-set turtles-on neighbors self) with [rule = my-rule]/ count (turtle-set turtles-on neighbors self)      
          ifelse (top - bottom) = 0
          [set satisfaction 1]
          [set satisfaction (my-group - bottom) / (top - bottom)]  
      
        ]
    if rule = 4 ;; si règle anticonf
      [
          let top-rule one-of majority-rules
          let top count (turtle-set turtles-on neighbors self) with [rule = top-rule]/ count (turtle-set turtles-on neighbors self) 
          let bottom-rule one-of minority-rules
          let bottom count (turtle-set turtles-on neighbors self) with [rule = bottom-rule]/ count     (turtle-set turtles-on neighbors self)     
          let my-rule rule
          let my-group count (turtle-set turtles-on neighbors self) with [rule = my-rule]/ count     (turtle-set turtles-on neighbors self)      
          ifelse (top - bottom) = 0
          [set satisfaction 1]
          [set satisfaction (top - my-group) / (top - bottom)]  
         
         
         ; let top-rule one-of majority-rules
         ; let bottom   -1 * count (turtle-set turtles-on neighbors self) with [rule = top-rule]/ count     (turtle-set turtles-on neighbors self) 
         ; let bottom-rule one-of minority-rules
         ; let top -1 * count (turtle-set turtles-on neighbors self) with [rule = bottom-rule]/ count     (turtle-set turtles-on neighbors self)     
         ; let my-rule rule
         ; let my-group -1 * count (turtle-set turtles-on neighbors self) with [rule = my-rule]/       count (turtle-set turtles-on neighbors self)      
         ; ifelse (top - bottom) = 0
         ; [set satisfaction 1]
         ; [set satisfaction (my-group - bottom) / (top - bottom)] 
        ]
    ]
    if not any? (turtles-on neighbors) ;; s’il n’y a pas de tortues voisines
  [set satisfaction 0]
end



to moving-stage
   ask turtles [if move? 
     [ifelse auto
     [if  (not am-i-progressing?) [move-agent]]
     [if (not am-i-the-best?) [move-agent]]
     
   ]];; si on peut bouger et qu’on est pas le meilleur alors on bouge ; or satisfaction < 0.5
end


to reset-decisions
  ask turtles [
  set move? false
  set rule? false
  set behavior? false
  ] 
end   


to replacement
  ask turtles [    
     let index1  age   
     let index2 (age  + 1)       
     let ex1 (1 - item index1 cumulative-census-dist) ;;  proba d'etre vivant à l'age de l'agent
     let ex2 (1 - item index2 cumulative-census-dist) ;;  proba d'etre vivant à l'age de l'agent + 1
     let prob-death 0
     ifelse ex1 > 0
        [set prob-death   (ex1 - ex2)/( ex1 ) ]
        [set prob-death 1]       
     if  random-float 1  < prob-death 
  [replace]
  
  ]  
end   



to replace  
    set rule (random 4) + 1 ;; donne une stratégie à chaque tortue       
    ifelse random-float 1.0 < (inicoop / 100)  ;; si la coopération initiale de la tortue est supérieure à un entier alors ont dit qu’il coopère sinon on dit qu’il ne coopère pas  
        [set cooperate? true]
        [set cooperate? false]
     set likelihood-to-move Initial-like-to-move
     set theta_1 Initial-prob-update-behavior
     set theta_2 Initial-prob-update-rule
     set weighting-history Initial-weighting-history               
     set score 0.0
     set score2 0.0
     set last-score 0.0
     set auto auto_biographic_agents
     ifelse auto         
         [set shape "target"] ;; chaque tortue est contente au départ
         [set shape "face happy"]           
     set rule? false  
     set behavior? false  
     set move? false      
     set age 0
     set t 0   
end


to set-outputs
    set cooperation-rate count turtles with [cooperate?] / count turtles
    set fraction-best count turtles with [shape = "face happy" or shape = "face target"]/ count turtles
    set satisfaction-rate mean [satisfaction] of turtles
    
   set maxi-auto count turtles with [rule = 1] with [auto = true] / count turtles
   set mini-auto count turtles with [rule = 2] with [auto = true] / count turtles
   set conf-auto count turtles with [rule = 3] with [auto = true] / count turtles
   set anti-auto count turtles with [rule = 4] with [auto = true] / count turtles
   ; set maxi count turtles with [rule = 1] with [auto = false] / count turtles
   ; set mini count turtles with [rule = 2] with [auto = false] / count turtles
   ; set conf count turtles with [rule = 3] with [auto = false] / count turtles
   ; set anti count turtles with [rule = 4] with [auto = false] / count turtles
    
    set maxi count turtles with [rule = 1] / count turtles
    set mini count turtles with [rule = 2] / count turtles
    set conf count turtles with [rule = 3] / count turtles
    set anti count turtles with [rule = 4] / count turtles
    
    set theta1-mean-maxi sum [theta_1] of turtles with [rule = 1] / count turtles with [rule = 1]
    set theta1-mean-mini sum [theta_1] of turtles with [rule = 2] / count turtles with [rule = 2]
    set theta1-mean-conf sum [theta_1] of turtles with [rule = 3] / count turtles with [rule = 3]
    set theta1-mean-anti sum [theta_1] of turtles with [rule = 4] / count turtles with [rule = 4]
    set theta2-mean-maxi sum [theta_2] of turtles with [rule = 1] / count turtles with [rule = 1]
    set theta2-mean-mini sum [theta_2] of turtles with [rule = 2] / count turtles with [rule = 2]
    set theta2-mean-conf sum [theta_2] of turtles with [rule = 3] / count turtles with [rule = 3]
    set theta2-mean-anti sum [theta_2] of turtles with [rule = 4] / count turtles with [rule = 4]
    
    set ratio-maxi theta1-mean-maxi / theta2-mean-maxi
    set ratio-mini theta1-mean-mini / theta2-mean-mini
    set ratio-conf theta1-mean-conf / theta2-mean-conf
    set ratio-anti theta1-mean-anti / theta2-mean-anti
    
    set satisfaction-mean-maxi sum [satisfaction] of turtles with [rule = 1] / count turtles with [rule = 1]
    set satisfaction-mean-mini sum [satisfaction] of turtles with [rule = 2] / count turtles with [rule = 2]
    set satisfaction-mean-conf sum [satisfaction] of turtles with [rule = 3] / count turtles with [rule = 3]
    set satisfaction-mean-anti sum [satisfaction] of turtles with [rule = 4] / count turtles with [rule = 4]
    
    
    
    
ifelse count turtles with [rule = 1] with [auto = true] = 0
    [set mean-age-auto-maxi 0]
    [set mean-age-auto-maxi sum [age] of turtles with [rule = 1] with [auto = true] / count turtles with [rule = 1] with [auto = true]]
    
ifelse count turtles with [rule = 2] with [auto = true] = 0    
    [set mean-age-auto-mini 0]
    [set mean-age-auto-mini sum [age] of turtles with [rule = 2] with [auto = true] / count turtles with [rule = 2] with [auto = true]]
    
ifelse count turtles with [rule = 3] with [auto = true] = 0    
    [set mean-age-auto-conf 0]
    [set mean-age-auto-conf sum [age] of turtles with [rule = 3] with [auto = true] / count turtles with [rule = 3] with [auto = true]]
    
ifelse count turtles with [rule = 4] with [auto = true] = 0   
    [set mean-age-auto-anti 0]
    [set mean-age-auto-anti sum [age] of turtles with [rule = 4] with [auto = true] / count turtles with [rule = 4] with [auto = true]]
    
ifelse count turtles with [rule = 1] with [auto = false] = 0    
    [set mean-age-auto-maxi 0]
    [set mean-age-autre-maxi sum [age] of turtles with [rule = 1] with [auto = false] / count turtles with [rule = 1] with [auto = false]]      
ifelse count turtles with [rule = 2] with [auto = false] = 0    
    [set mean-age-auto-mini 0]
    [set mean-age-autre-mini sum [age] of turtles with [rule = 2] with [auto = false] / count turtles with [rule = 2] with [auto = false]]
      
ifelse count turtles with [rule = 3] with [auto = false] = 0
    [set mean-age-auto-conf 0]
    [set mean-age-autre-conf sum [age] of turtles with [rule = 3] with [auto = false] / count turtles with [rule = 3] with [auto = false]]
      
ifelse count turtles with [rule = 4] with [auto = false] = 0
    [set mean-age-auto-anti 0]
    [set mean-age-autre-anti sum [age] of turtles with [rule = 4] with [auto = false] / count turtles with [rule = 4] with [auto = false]]
    
    set progres count turtles with [auto] / count turtles
    set n-progres count turtles with [ not auto] / count turtles

end


to update-plot
  set-current-plot "cooperation"
  set-current-plot-pen "cooperation"
  plot cooperation-rate
  set-current-plot-pen "satisfaction"
  plot satisfaction-rate
  set-current-plot-pen "fraction-best"
  plot fraction-best 
  
  set-current-plot "population"
  set-current-plot-pen "maxi"
  plot maxi
  set-current-plot-pen "mini"
  plot mini
  set-current-plot-pen "conf"
  plot conf
  set-current-plot-pen "anti"
  plot anti
 ; set-current-plot-pen "maxi-auto"
 ; plot maxi-auto
 ; set-current-plot-pen "mini-auto"
 ; plot mini-auto
 ; set-current-plot-pen "conf-auto"
 ; plot conf-auto
 ; set-current-plot-pen "anti-auto"
 ; plot anti-auto
  
  
  set-current-plot "strategy details"
  set-current-plot-pen "rule update"
  plot count turtles with [rule?] / count turtles
  set-current-plot-pen "% moving"
  plot count turtles with [move?] / count turtles
  
  set-current-plot "ratiotheta1/theta2"
  plot ratio-maxi
  set-current-plot-pen "ratio-maxi"
  plot ratio-maxi 
  set-current-plot-pen "ratio-mini"
  plot ratio-mini
  set-current-plot-pen "ratio-conf"
  plot ratio-conf
  set-current-plot-pen "ratio-anti"
  plot ratio-anti
  
 set-current-plot "satisfaction2"
  set-current-plot-pen "satisfaction_maxi"
  plot satisfaction-mean-maxi
  set-current-plot-pen "satisfaction_mini"
  plot satisfaction-mean-mini
  set-current-plot-pen "satisfaction_conf"
  plot satisfaction-mean-conf
  set-current-plot-pen "satisfaction_anti"
  plot satisfaction-mean-anti 
  
  set-current-plot "comparaison"
  set-current-plot-pen "auto"
  plot progres
  set-current-plot-pen "autres"
  plot n-progres
  end


to update-views
ask turtles [establish-color]
end

to establish-color  ;; couleur tortues en fiction de leur règle
  
  if rule = 1 
    [set color red
      ]
 
  if rule = 2 
    [set color green
      ]
  
  if rule = 3 
    [set color blue
      ]
  
  if rule = 4 
    [set color white
      ]
  
    
end




to init-age-FR2011 ;; initialisation de l’age grâce à la pyramide des ages
  let census-dist (list 0.01180683 0.01201037 0.01209845 0.01234063 0.01227253 0.01234422 0.01227801 0.01250872 0.01225243 0.01220084 0.01216063 0.01225841 0.01243949 0.01269816 0.01215054 0.01210297 0.01204218 0.01225426 0.01215026 0.01175250 0.01158990 0.01192538 0.01187672 0.01193472 0.01186481 0.01195931 0.01191443 0.01209404 0.01205728 0.01199851 0.01184490 0.01265457 0.01279754 0.01296898 0.01230444 0.01208202 0.01224300 0.01196657 0.01234141 0.01307640 0.01381147 0.01418162 0.01408421 0.01379278 0.01357054 0.01350406 0.01343341 0.01375896 0.01378147 0.01394149 0.01374521 0.01330135 0.01333475 0.01326854 0.01320184 0.01288553 0.01284952 0.01274903 0.01264137 0.01262325 0.01233349 0.01253659 0.01219221 0.01267168 0.01243927 0.01241485 0.01210161 0.01147030 0.00860238 0.00839324 0.00814365 0.00754129 0.00669977 0.00695507 0.00727191 0.00710799 0.00690240 0.00684548 0.00662450 0.00661433 0.00621870 0.00621606 0.00582400 0.00560822 0.00497965 0.00458912 0.00415829 0.00377767 0.00338402 0.00289983 0.00250650 0.00215533 0.00181251 0.00149459 0.00069600 0.00047605 0.00032965 0.00022167 0.00018668 0.00020581 0.00013013 0.00007982 0.00005118 0.00003188 0.00001312 0.00001378)
  set cumulative-census-dist (list 0.011807 0.023817 0.035916 0.048256 0.060529 0.072873 0.085151  0.09766  0.10991  0.12211  0.13427  0.14653  0.15897  0.17167  0.18382  0.19592  0.20797  0.22022  0.23237  0.24412  0.25571  0.26764  0.27951  0.29145  0.30331  0.31527  0.32719  0.33928  0.35134  0.36334  0.37518  0.38784  0.40063   0.4136  0.42591  0.43799  0.45023   0.4622  0.47454  0.48762  0.50143  0.51561  0.52969  0.54349  0.55706  0.57056 0.584  0.59775  0.61154  0.62548  0.63922  0.65252  0.66586  0.67913  0.69233  0.70521  0.71806  0.73081  0.74345  0.75608  0.76841  0.78095  0.79314  0.80581  0.81825  0.83067  0.84277  0.85424  0.86284  0.87123  0.87938  0.88692  0.89362  0.90057  0.90785  0.91495  0.92186   0.9287  0.93533  0.94194  0.94816  0.95437   0.9602  0.96581  0.97079  0.97538  0.97953  0.98331   0.9867   0.9896   0.9921  0.99426  0.99607  0.99756  0.99826  0.99874  0.99907  0.99929  0.99947  0.99968  0.99981  0.99989  0.99994  0.99997  0.99999  1 1) ; on ajoute un 1 à la fin pour que la différence entre le dernier et l'avant dernier item soit 0 i.e. la proba de survivre entre ces deux pas de temps est 0
 ; let cumulative-census-dist (list 0.011807 0.023817 0.035916 0.048256 0.060529 0.072873 0.085151  0.09766  0.10991  0.12211  0.13427  0.14653  0.15897  0.17167  0.18382  0.19592  0.20797  0.22022  0.23237  0.24412  0.25571  0.26764  0.27951  0.29145  0.30331  0.31527  0.32719  0.33928  0.35134  0.36334  0.37518  0.38784  0.40063   0.4136  0.42591  0.43799  0.45023   0.4622  0.47454  0.48762  0.50143  0.51561  0.52969  0.54349  0.55706  0.57056 0.584  0.59775  0.61154  0.62548  0.63922  0.65252  0.66586  0.67913  0.69233  0.70521  0.71806  0.73081  0.74345  0.75608  0.76841  0.78095  0.79314  0.80581  0.81825  0.83067  0.84277  0.85424  0.86284  0.87123  0.87938  0.88692  0.89362  0.90057  0.90785  0.91495  0.92186   0.9287  0.93533  0.94194  0.94816  0.95437   0.9602  0.96581  0.97079  0.97538  0.97953  0.98331   0.9867   0.9896   0.9921  0.99426  0.99607  0.99756  0.99826  0.99874  0.99907  0.99929  0.99947  0.99968  0.99981  0.99989  0.99994  0.99997  0.99999  1)
ask turtles [
   let temp-init random-float 1
   let temp-age 0
    while [item temp-age cumulative-census-dist < temp-init][set temp-age ( temp-age + 1 )]    
    set age temp-age
]
end



to set-life-distribution-FR2011 ;;Life expectation for ages according data colected by INSEE 2013 France métropolitaine                              
 ; set life-distribution (list ((81.32 + 80.61 + 79.63 + 78.64 + 77.65) / 5) ((76.66 + 75.67 + 74.68 + 73.68 + 72.69) / 5) ((71.69 + 70.70 + 69.70 + 68.71 + 67.72) / 5) ((66.73 + 65.74 + 64.76 + 63.77 + 62.80) / 5) ((61.83 + 60.85 + 59.88 + 58.91 + 57.94) / 5) ((56.97 + 56.00 + 55.04 + 54.07 + 53.10) / 5) ((52.13 + 51.16 + 50.19 + 49.23 + 48.26) / 5) ((47.30 + 46.34 + 45.38 + 44.43 + 43.48) / 5) ((42.53 + 41.59 + 40.65 + 39.71 + 38.78) / 5) ((37.86 + 36.94 + 36.02 + 35.11 + 34.21) / 5) ((33.32 + 32.44 + 31.56 + 30.70 + 29.84) / 5) ((28.99 + 28.14 + 27.30 + 26.47 + 25.64) / 5) ((24.82 + 24.00 + 23.18 + 22.37 + 21.56) / 5) ((20.76 + 19.97 + 19.18 + 18.39 + 17.61) / 5) ((16.83 + 16.07 + 15.31 + 14.56 + 13.83) / 5) ((13.10 + 12.39 + 11.70 + 11.02 + 10.36) / 5) ((9.72 + 9.10 + 8.51 + 7.94 + 7.39) / 5) ((6.87 + 6.37 + 5.91 + 5.48 + 5.07) / 5) ((4.69 + 4.35 + 4.02 + 3.70 + 3.42) / 5) ((3.17 + 2.94 + 2.75 + 2.57 + 2.40) / 5) )
  ;set life-distribution (list 81.32 80.61 79.63 78.64 77.65 76.66 75.67 74.68 73.68 72.69 71.69 70.70 69.70 68.71 67.72 66.73 65.74 64.76 63.77 62.80 61.83 60.85 59.88 58.91 57.94 56.97 56.00 55.04 54.07 53.10 52.13 51.16 50.19 49.23 48.26 47.30 46.34 45.38 44.43 43.48 42.53 41.59 40.65 39.71 38.78 37.86 36.94 36.02 35.11 34.21 33.32 32.44 31.56 30.70 29.84 28.99 28.14 27.30 26.47 25.64 24.82 24.00 23.18 22.37 21.56 20.76 19.97 19.18 18.39 17.61 16.83 16.07 15.31 14.56 13.83 13.10 12.39 11.70 11.02 10.36 9.72 9.10 8.51 7.94 7.39 6.87 6.37 5.91 5.48 5.07 4.69 4.35 4.02 3.70 3.42 3.17 2.94 2.75 2.57 2.40 )
 set life-distribution (list 0.0003 0.0001 0.0001 0.0002 0.0005 0.0005 0.0006 0.0009 0.0014 0.0023 0.0038 0.0058 0.008 0.0108 0.0206 0.0634 0.190 )
 
end



to-report majority-rules  ;; reports a set with the number of the most frequent rules in agent's neighborhood (agent included)
                          ;; be careful when use in an ask cycle as the command is applied to "self"
  let mylist [rule] of (turtle-set turtles-on neighbors self)
  set mylist modes mylist
  report mylist
end


to-report minority-rules ;; reports a set with the number of the less frequent rules in agent's neighborhood (agent included)
                         ;; be careful when use in an ask cycle as the command is applied to "self"
  let mylist_1 [rule] of (turtle-set turtles-on neighbors self)
  let mylist []
  let j 1
  while [empty? mylist] [
  let i 1
  repeat 4 [
    if length filter [? = i] mylist_1 = j  [set mylist lput i mylist] 
    set i i + 1
    ]
  set j j + 1
  ] 
  report mylist
end


to-report majority-rules-global  ;; reports a set with the number of the most frequent rules in agent's neighborhood (agent included)
                          ;; be careful when use in an ask cycle as the command is applied to "self"
  let mylist [rule] of turtles
  set mylist modes mylist
  report mylist
end




to-report minority-rules-global ;; reports a set with the number of the less frequent rules in agent's neighborhood (agent included)
                         ;; be careful when use in an ask cycle as the command is applied to "self"
  let mylist_1 [rule] of turtles
  let mylist []
  let j 1
  while [empty? mylist] [
  let i 1
  repeat 4 [
    if length filter [? = i] mylist_1 = j  [set mylist lput i mylist] 
    set i i + 1
    ]
  set j j + 1
  ] 
  report mylist
end



to-report majority-behavior 
  let mylist [cooperate?] of (turtle-set turtles-on neighbors self)
  report one-of modes mylist
end


to-report am-i-the-best? ;; reports true if the agents is the best in its neighborhood (according with its rule) and false otherwise
  let test false
  if any? turtles-on neighbors [
  if (rule = 1) and (score >= [score] of max-one-of turtles-on neighbors [score] * 0.99) [set test true]
  if (rule = 2) and (score2 <= [score2] of min-one-of turtles-on neighbors [score2] * 1.01) [set test true]
  ;if (rule = 1) and (score >= [score] of max-one-of turtles-on neighbors [score]) [set test true]
  ;if (rule = 2) and (score2 <= [score2] of min-one-of turtles-on neighbors [score2]) [set test true]
  if (rule = 3) and (member? rule majority-rules) [set test true]
  if (rule = 4) and (member? rule minority-rules) and not all? (turtles-on neighbors) [rule = 4] [set test true]  
  ]
  report test
end

to-report am-i-progressing?
  let test false
  if (rule = 1) and (score > last-score)
    [set test true
     ; set auto true
     ]
  if (rule = 2) and (score2 > last-score2)
    [set test true
     ; set auto true
     ]
  if (rule = 3) and count-majority-rule > last-count-majority-rule
    [set test true
     ; set auto true
     ]
  if (rule = 4) and count-minority-rule < last-count-minority-rule
    [set test true
    ;  set auto true
    ]
  report test
end

to-report am-i-the-best-global?
let test false

  if (rule = 1) and (score >= [score] of max-one-of turtles [score] * 0.99) 
     [set test true]
  if (rule = 2) and (score <= [score] of min-one-of turtles [score] * 1.01) 
     [set test true]
  if (rule = 3) and (member? rule majority-rules-global) and all? turtles [rule = 3] 
     [set test true]
  if (rule = 4) and (member? rule minority-rules-global) and not all? turtles [rule = 4] 
     [set test true]  
  

report test
end


to-report best-elements ;; report a list with the agents with the best performance according agents
  
  let myset (turtle-set turtles-on neighbors self)
  if rule = 1 [set myset myset with [score >= [score] of max-one-of myset [score] * 0.99]]
  
  if rule = 2 [set myset myset with [score <= [score] of min-one-of myset [score] * 1.1]]
  if rule = 3 [
    let rules-list majority-rules
    set myset myset with [member? rule rules-list]
    ] 
  if rule = 4 [
    let rules-list minority-rules
    if not empty? rules-list [
    set myset myset with [member? rule rules-list]
    ]  
  ]
  report myset
end  


to-report best-elements-global ;; report a list with the agents with the best performance according agents
  
  let myset turtles ;; myset : l'espace de toutes les tortues
  if rule = 1 [set myset myset with [score >= [score] of max-one-of myset [score] * 0.99]]
  
  if rule = 2 [set myset myset with [score <= [score] of min-one-of myset [score] * 1.1]]
  if rule = 3 [
    let rules-list majority-rules-global
    set myset myset with [member? rule rules-list]
    ] 
  if rule = 4 [
    let rules-list minority-rules-global
    if not empty? rules-list [
    set myset myset with [member? rule rules-list]
    ]  
  ]
  report myset
end 


to-report is-my-rule-the-best? ;; reports true if the agent's rule is used by any of the best valuated agents in its neighborhood (according with its rule) and false otherwise
  let test false
  ifelse am-i-the-best? [set test true][
  if member? rule [rule] of best-elements [set test true] 
  ]
  report test
end



to select-rule              
   if not is-my-rule-the-best?   
   [copy-strategy (one-of best-elements)]         
end



to copy-strategy [temp-agent]
  
      ifelse  random-float 1.0 > erreur-inference-regle
         [
           set rule [rule] of temp-agent
           set auto [auto] of temp-agent
          ]
         [set rule random 4 + 1
          ifelse random 1 < 0.5
          [set auto true]
          [set auto false]
         ] 
      
      ifelse  random-float 1.0 > erreur-inference-comportement
         [
           set cooperate? [cooperate?] of temp-agent
          ]
         [
          ifelse random 1 < 0.5
          [set cooperate? true]
          [set cooperate? false]
         ] 
      
      set theta_1 [theta_1] of temp-agent 
      set theta_1 add-noise "theta_1" Transcription-error
            
      set theta_2 [theta_2] of temp-agent 
      set theta_2 add-noise "theta_2" Transcription-error           
      set weighting-history [weighting-history] of temp-agent
     ;set weighting-history add-noise "weighting-history" Transcription-error             
      set likelihood-to-move [likelihood-to-move] of temp-agent 
      set likelihood-to-move add-noise "likelihood-to-move" Transcription-error 
       
end


to-report add-noise [value noise-std]
      let epsilon random-normal 0.0 noise-std
      if ( epsilon <= -100 )
      [ set epsilon -99] 
      let noisy-value runresult value * 100 / (100 + epsilon)
      if (noisy-value > 1) [set noisy-value 1]
      if (noisy-value < 0) [set noisy-value 0]     
      report noisy-value
end


to select-behavior  ;; patch procedure
  if any? turtles-on neighbors
  [
    ifelse  random-float 1.0 < erreur-inference-comportement
    
    [
      ifelse random-float 1.0  < 0.5
       [set cooperate? true]
       [set cooperate? false]
      ]
    
    
  [
    if (rule = 1)  
  [set cooperate? [cooperate?] of one-of best-elements]
   if (rule = 2)  
  [set cooperate? [cooperate?] of one-of best-elements]                                                              ;;choose behavior (cooperate. not cooperate)
                                                                ;; of neighbor who performed best according
                                                                ;; the agent's rule 
  if rule = 3
  [set cooperate? majority-behavior]                                                              
  if rule = 4 
  [set cooperate? not majority-behavior]
  ] 
  ]  
end


to move-agent
 
   if any? turtles-on neighbors ;; s’il y a conflict 
    [
      let target one-of other best-elements
      if target = nobody 
  [set target one-of turtles-on neighbors] ;; s’il n’y a pas de target on dit que target est un des voisins de la tortue
      ifelse random-float 1 < (1 - satisfaction)*(1 - [satisfaction] of target) 
      [interchange-agents target] ;; si
      [move-to-empty] ;; sinon exécuter move-to-empty
    ]
end




to move-to-empty
  if any? neighbors with [not any? turtles-here] ;; si il y a des voisinages sans tortue
  [move-to one-of neighbors with [not any? turtles-here]] ;; bouger la tortue vers le voisinage sans tortue
end


to interchange-agents [my-target]
   let my-patch patch-here
   let target-patch patch-here
   ask my-target [
     set target-patch patch-here
     move-to my-patch
     ]  
   move-to target-patch 
end

to data
  
print cooperation-rate 
print satisfaction-rate
print fraction-best

print maxi
print mini
print conf
print anti
print maxi-auto
print mini-auto 
print conf-auto
print anti-auto

print ratio-maxi
print ratio-mini
print ratio-conf
print ratio-anti

print satisfaction-mean-maxi
print satisfaction-mean-mini
print satisfaction-mean-conf
print satisfaction-mean-anti

print mean-age-auto-conf
print mean-age-auto-anti
print mean-age-auto-maxi
print mean-age-auto-mini
print mean-age-autre-maxi
print mean-age-autre-mini
print mean-age-autre-conf
print mean-age-autre-anti

print progres
print n-progres

end
