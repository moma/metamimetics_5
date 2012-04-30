extensions [network]

turtles-own
[
  node-clustering-coefficient
  distance-from-other-turtles   ;; list of distances of this node from other turtles
  neighborhood
  cooperate?       ;; patch will cooperate
  rule             ;; patch will have one of four rules: 1=Maxi 2=mini 3=conformist 4=anticonformist
  score            ;; score resulting from interaction of neighboring patches. It is dictated by the PD payoffs and the discount factor
  last-score
  inst-score
  rule?
  behavior?
  rewire?
  likelihood-to-rewire
  age
  weighting-history

  changed-neighborhood?
]

links-own
[
    rewired?                  ;; keeps track of whether the link has been rewired or not
]

globals
[
  clustering-coefficient               ;; the clustering coefficient of the network; this is the
                                       ;; average of clustering coefficients of all turtles
  average-path-length                  ;; average path length of the network
  clustering-coefficient-of-lattice    ;; the clustering coefficient of the initial lattice
  average-path-length-of-lattice       ;; average path length of the initial lattice
  infinity                             ;; a very large number.
                                         ;; used to denote distance between two turtles which
                                         ;; don't have a connected or unconnected path between them
  highlight-string                     ;; message that appears on the node properties monitor
  number-rewired                       ;; number of edges that have been rewired. used for plots.
  rewire-one?                          ;; these two variables record which button was last pushed
  
  rewire-all?
  
  cooperation-rate
  fraction-best-maxi
  fraction-best-mini
  fraction-best-conf
  fraction-best-anti
  maxi
  mini
  conf
  anti
  worked?
  life-distribution
]

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to startup
  set highlight-string ""
end

to setup

  clear-all
  reset-ticks
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)

  set infinity 99999  ;; just an arbitrary choice for a large number
  set-default-shape turtles "circle"
  make-turtles

  ;; set up a variable to determine if we still have a connected network
  ;; (in most cases we will since it starts out fully connected)
  let success? false
  while [not success?] [
    ;; we need to find initial values for lattice
    wire-them
    ;;calculate average path length and clustering coefficient for the lattice
    set success? do-calculations
  ]
  set average-path-length network:mean-link-path-length turtles links
  ;; setting the values for the initial lattice
  set clustering-coefficient-of-lattice clustering-coefficient
  set average-path-length-of-lattice average-path-length
  set number-rewired 0
  set highlight-string ""
  rewire-all
  set average-path-length network:mean-link-path-length turtles links
  ask turtles [set neighborhood link-neighbors]
  ask turtles [
    set rule (random 4) + 1 
    
    set changed-neighborhood? false
    set score 0.0
    set rule? false
    set behavior? false
    set rewire? false
    set age 0
      set shape "face happy"
      ifelse random-float 1.0 < (inicoop / 100)
        [set cooperate? true]
        [set cooperate? false]
  ]
  ifelse random-init [
    ask turtles[
;;      set theta_1 random-float 1.0
 ;     set theta_2 random-float 1.0
      set weighting-history random-float 1
      set likelihood-to-rewire random-float 0.05
    ]
        ]  
      [ask turtles[
  ;    set theta_1 Initial-prob-update-behavior
   ;   set theta_2 Initial-prob-update-rule
      set weighting-history initial-weighting-history
      set likelihood-to-rewire Initial-likelihood-to-rewire]
      ]
  ask turtles [establish-color]
  init-age-USA2007
  ifelse timescale = "months" [set-life-distribution-USA2007-months][set-life-distribution-USA2007] 
  ;ask turtles [interact]
  
reset-ticks
  
  
end

to go

  ask turtles [interact] 
    decision-stage
    learning-stage
    rewiring-stage
    do-plotting
  ask turtles [
    ifelse am-i-the-best? [set shape "face happy"][set shape "face sad"]
    ]  
  update-views
  change-layout
  set-outputs
  reset-decisions 
  if replacement? [replacement]
  redo-plots

  tick
    

end


to make-turtles
  crt num_nodes [ set color gray + 2 ]
  ;; arrange them in a circle in order by who number
  layout-circle (sort turtles) max-pxcor - 1
  ;;layout-radial turtles links (turtle 0)


end


to establish-color  ;; agent procedure
  if rule = 1 
    [set color 13 + score / 2
      if color > 19.9 [set color 19.9]
      ]
  if rule = 2
    [set color 53 + score / 2
       if color > 59.9 [set color 59.9]
      ]
  if rule = 3
    [set color 93 + score / 2
       if color > 99.9 [set color 99.9]
      ]
  if rule = 4  
    [set color 3 + score / 2
       if color > 9.9 [set color 9.9]
      ]
  ifelse cooperate?
    [set size 1.2]
    [set size 0.8]
end


to update-views
  ask turtles [establish-color]
   ; find-path-lengths

  ;let num-connected-pairs sum [length remove infinity (remove 0 distance-from-other-turtles)] of turtles
  ;set average-path-length (sum [sum distance-from-other-turtles] of turtles) / (num-connected-pairs)
  set worked? do-calculations
 end


to decision-stage
 
   ask turtles [ 
      ifelse random-float 1 < likelihood-to-rewire 
   [if not am-i-the-best? and ((age > 12 and ticks > 10) or not Maturing-period ) [set rewire? true]]
   [
     ifelse not am-i-the-best? and not is-my-rule-the-best? and ((age > 12 and ticks > 10) or not Maturing-period ) [set rule? true]
     [if not am-i-the-best? [set behavior? true]]
   ]
   ]
end

to learning-stage
    ask turtles [ 
   if rule?   
   [     
       select-rule
       ;establish-color
       select-behavior
   ]
   if behavior? [select-behavior]
   ]
end


;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Procedure ;;;
;;;;;;;;;;;;;;;;;;;;;;;


to rewire-all

  ;; make sure num-turtles is setup correctly; if not run setup first
  if count turtles != num_nodes [
    setup
  ]

  ;; record which button was pushed
  set rewire-one? false
  set rewire-all? true

  ;; set up a variable to see if the network is connected
  let success? false

  ;; if we end up with a disconnected network, we keep trying, because the APL distance
  ;; isn't meaningful for a disconnected network.
  while [not success?] [
    ;; kill the old lattice, reset neighbors, and create new lattice
    ask links [ die ]
    wire-them
    set number-rewired 0

    ask links [

      ;; whether to rewire it or not?
      if (random-float 1) < rewiring_probability
      [
        ;; "a" remains the same
        let node1 end1
        ;; if "a" is not connected to everybody
        if [ count link-neighbors ] of end1 < (count turtles - 1)
        [
          ;; find a node distinct from node1 and not already a neighbor of node1
          let node2 one-of turtles with [ (self != node1) and (not link-neighbor? node1) ]
          ;; wire the new edge
          ask node1 [ create-link-with node2 [ set color cyan  set rewired? true ] ]

          set number-rewired number-rewired + 1  ;; counter for number of rewirings
          set rewired? true
        ]
      ]
      ;; remove the old edge
      if (rewired?)
      [
        die
      ]
    ]

    ;; check to see if the new network is connected and calculate path length and clustering
    ;; coefficient at the same time
    set success? do-calculations
  ]

  ;; do the plotting
  do-plotting
end



to interact  ;; calculates the agent's payoff for Prisioner's Dilema. Each agents plays only with its neighbors
  set neighborhood link-neighbors       
  let total-cooperators count (turtles-on neighborhood) with [cooperate?]
  set inst-score 0
  ifelse cooperate?
  ;[set inst-score total-cooperators + 1]
  ;[set inst-score total-cooperators * strength_of_dilemma]
    [set inst-score total-cooperators * ( 1 - strength_of_dilemma)]                   ;; cooperator gets score of # of neighbors who cooperated
    [set inst-score total-cooperators + (count (turtles-on neighborhood) - total-cooperators) * strength_of_dilemma ]  ;; non-cooperator get score of a multiple of the neighbors who cooperated
  set last-score score
  ifelse count turtles-on neighborhood = 0[set inst-score 0][
  set inst-score ((inst-score * 8 / count turtles-on neighborhood ) )]
  
  set score inst-score * ( 1 - weighting-history) + last-score * weighting-history   
  
  ;set score inst-score  
end

to-report am-i-the-best? ;; reports true if the agents is the best in its neighborhood (according with its rule) and false otherwise
  let test false
  ;; In the model, an isolated agent can not consider himself as the best
  if any? turtles-on neighborhood [
  if (rule = 1) and (score >= [score] of max-one-of turtles-on neighborhood [score] * 0.99) [set test true]
  if (rule = 2) and (score <= [score] of min-one-of turtles-on neighborhood [score] * 1.01) [set test true]
  if (rule = 3) and (member? rule majority-rules) [set test true]
  if (rule = 4) and (member? rule minority-rules) and not all? (turtles-on neighborhood) [rule = 4] [set test true]  
  ]
  report test
end


to-report is-my-rule-the-best? ;; reports true if the agent's rule is used by any of the best valuated agents in its neighborhood (according with its rule) and false otherwise
  let test false
  ifelse am-i-the-best? [set test true][
  if member? rule [rule] of best-elements [set test true] 
  ]
  report test
end


to select-rule
                 ;; the agent changes its rule if every more succesfull neighbor has a different rule (if them exist).
                 ;; The agent never change his rule nor behavior if is in the set of agents with best performance (according its rule)
       
     if not am-i-the-best? 
     [
     if not is-my-rule-the-best? [
      copy-strategy (one-of best-elements)
       ]
     ] 
end


to select-behavior  ;; patch procedure
  if any? turtles-on neighborhood
  [
  if (rule = 1) or (rule = 2) 
  [set cooperate? [cooperate?] of one-of best-elements]
                                                                ;;choose behavior (cooperate, not cooperate)
                                                                ;; of neighbor who performed best according
                                                                ;; the agent's rule 
  
  if rule = 3
  [set cooperate? majority-behavior]                                                              
  if rule = 4 
  [set cooperate? not majority-behavior]
  ]   
end


to-report majority-rules  ;; reports a set with the number of the most frequent rules in agent's neighborhood (agent included)
                          ;; be careful when use in an ask cycle as the command is applied to "self"
  let mylist [rule] of (turtle-set turtles-on neighborhood self)
  set mylist modes mylist
  report mylist
end


to-report minority-rules ;; reports a set with the number of the less frequent rules in agent's neighborhood (agent included)
                         ;; be careful when use in an ask cycle as the command is applied to "self"
  let mylist_1 [rule] of (turtle-set turtles-on neighborhood self)
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


to-report best-elements ;; report a list with the agents with the best performance according agents
  
  let myset (turtle-set turtles-on neighborhood self)
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

to copy-strategy [temp-agent]
  
  ifelse random-float 1 < Transcription-error[
    let new-rule (random 4) + 1
    while [new-rule = [rule] of temp-agent]
    [
      set new-rule (random 4) + 1
    ]
    set rule new-rule
    ]
  [
    set rule [rule] of temp-agent]
  set likelihood-to-rewire [likelihood-to-rewire] of temp-agent 
;  set likelihood-to-rewire likelihood-to-rewire + random-normal 0 Transcription-error
  if likelihood-to-rewire < 0 [set likelihood-to-rewire 0]
      
     
end

to-report majority-behavior
  let mylist [cooperate?] of (turtle-set turtles-on neighborhood self)
  report one-of modes mylist
end



to set-outputs
    set cooperation-rate count turtles with [cooperate?] / count turtles
    if count turtles with [rule = 1] != 0 [
    
    set fraction-best-maxi count turtles with [shape = "face happy" and rule = 1]/ count turtles with [rule = 1]]
    if count turtles with [rule = 2] != 0 [
    set fraction-best-mini count turtles with [shape = "face happy" and rule = 2]/ count turtles with [rule = 2]]
    if count turtles with [rule = 3] != 0 [
    set fraction-best-conf count turtles with [shape = "face happy" and rule = 3]/ count turtles with [rule = 3]]
    if count turtles with [rule = 4] != 0 [
    set fraction-best-anti count turtles with [shape = "face happy" and rule = 4]/ count turtles with [rule = 4]]
  set maxi count turtles with [rule = 1] / count turtles
  set mini count turtles with [rule = 2] / count turtles
  set conf count turtles with [rule = 3] / count turtles
  set anti count turtles with [rule = 4] / count turtles
end



to redo-plots
  
  set-current-plot "cooperation"
  set-current-plot-pen "cooperation"
  plot cooperation-rate
  

  set-current-plot-pen "fraction-best-maxi"
  plot fraction-best-maxi
  set-current-plot-pen "fraction-best-mini"
  plot fraction-best-mini
  set-current-plot-pen "fraction-best-conf"
  plot fraction-best-conf
  set-current-plot-pen "fraction-best-anti"
  plot fraction-best-anti
  set-current-plot "population"
  set-current-plot-pen "maxi"
  
  plot maxi
  set-current-plot-pen "mini"
  plot mini
  set-current-plot-pen "conf"
  plot conf
  set-current-plot-pen "anti"
  plot anti
 
end


to rewiring-stage
   ask turtles [if rewire? and not am-i-the-best? [
       ifelse pref-rewiring?[
         rewire-pref][
         rewire-agent]] 
   ]
end

to rewire-agent
  ;let potential-neighbors link-neighbors
  let potential-neighbors link-neighbors with [not member? self best-elements]
  let potential-edges my-links with [member? other-end potential-neighbors]
  if any? potential-edges [
    ask one-of potential-edges [
            let node1 end1
            let node2 one-of turtles with [ (self != node1) and (not link-neighbor? node1) ]
            ask node1 [create-link-with node2]
            ask node1 [set changed-neighborhood? true]
            die]
  ]

end

to rewire-pref

  let i 2
  let list-dists []
  let num-eff count other turtles - count link-neighbors

  set list-dists lput (count other turtles network:in-link-radius 2 links - count link-neighbors) list-dists
  set i i + 1
  while [count other turtles network:in-link-radius i links != reduce + list-dists][
    set list-dists lput (count other turtles network:in-link-radius i links - reduce + list-dists) list-dists
    set i i + 1 
  ]
  let unconnected (count other turtles) - count link-neighbors - reduce + list-dists
  let last-item last list-dists
  set list-dists replace-item (length list-dists - 1) list-dists (unconnected + last-item)
  set list-dists map [? / (((position ? list-dists + 2) ^ 10) * num-eff)] list-dists 
  let sum-probs reduce + list-dists
  ifelse sum-probs = 0 [][
  

  set list-dists map [? / sum-probs] list-dists
  let temp-init (random length list-dists)
  let temp-agent one-of other turtles
  while [random-float 1 > item temp-init list-dists][set temp-init (random length list-dists) ]
  ;show temp-init
  ifelse temp-init = length list-dists - 1[
    let not-eligible other turtles network:in-link-radius (temp-init + 1) links
    let eligible other turtles with [not member? self not-eligible]
    set temp-agent one-of eligible
  ]
  [
  set temp-agent one-of other turtles network:in-link-radius (temp-init + 2) links
  while [network:link-distance temp-agent links != (temp-init + 2)][set temp-agent one-of other turtles network:in-link-radius (temp-init + 2) links]
  ]
  ;show network:link-distance temp-agent links
  
  let potential-neighbors link-neighbors with [not member? self best-elements]
  let potential-edges my-links with [member? other-end potential-neighbors]
  
  if any? potential-edges [
    ask one-of potential-edges [
            let node1 end1
            let node2 temp-agent
            ask node1 [create-link-with node2]
            ask node1 [set changed-neighborhood? true]
            die]
  ]]
end



to reset-decisions
  ask turtles [
  set rewire? false
  set rule? false
  set behavior? false
  ] 
end 

to set-life-distribution-USA2007 ;;Life expectation for ages according data colected by the Centers for Disease Control
                                 ;and Prevention’s National Center for Health Statistics (NCHS) USA 2010
                                 ;Murphy, Xu, and Kochanek 'Deaths: preliminary data 2010' National Vital Stat. Reports 60-4
                                 ;Reported ages have an interval of 5 years starting from 0 until 100 years 

  set life-distribution (list 0.0067375 0.000464 0.0002865 0.0002165 0.000174 0.0001575 0.000147 0.000137 0.000124 0.000107 9.45e-05 9.8e-05 0.0001325 0.0002045 0.000305 0.000415 0.000521 0.000621 0.000709 0.000785 0.000863 0.0009375 0.000988 0.001006 0.001 0.000987 0.000978 0.000976 0.0009865 0.0010085 0.001035 0.0010655 0.001104 0.00115 0.001207 0.0012735 0.001354 0.0014505 0.001566 0.0017 0.00185 0.002016 0.0022 0.0024015 0.002621 0.0028585 0.0031155 0.003395 0.0036985 0.0040245 0.0043835 0.0047625 0.005141 0.005511 0.005887 0.006297 0.006758 0.00727 0.007839 0.008471 0.009184 0.0099695 0.0108055 0.011686 0.0126375 0.0137105 0.014928 0.016282 0.0177855 0.0194565 0.021371 0.0235095 0.0257935 0.028209 0.030833 0.0338595 0.037323 0.04111 0.0452255 0.049783 0.055009 0.0609785 0.0676135 0.074947 0.083097 0.092204 0.102388 0.113735 0.126297 0.140086 0.155102 0.171327 0.188734 0.20729 0.226949 0.246645 0.266066 0.284872 0.302715 0.319238 0.33667 0.355063 0.374468 0.394942 0.416545 0.43934 0.463392 0.488773 0.515555 0.543816 0.57364 0.605114 0.638328 0.67338 0.710373 0.749416 0.789422 0.828894 0.870338 0.913855)
    
end
to set-life-distribution-USA2007-months
  set life-distribution (list
    0.00673750000000000  0.00482226617500628  0.00345146583489569  0.00247033572538884  0.00176810633164411  0.00126549600844554  0.000905760088479763  0.000648284413706369  0.000464000000000000  0.000436861538878166  0.000411310353773705  0.000387253607987293  0.000364603894658299  0.000343278919184046  0.000323201200213741  0.000304297788130701  0.000286500000000000  0.000276640834245358  0.000267120946499014  0.000257928661374858  0.000249052705265372  0.000240482192515447  0.000232206612071993  0.000224215814592964  0.000216500000000000  0.000210665936904966  0.000204989085321234  0.000199465208842909  0.000194090185223262  0.000188860003298461  0.000183770759994205  0.000178818657413011  0.000174000000000000  0.000171846488418622  0.000169719629780527  0.000167619094215472  0.000165544555935857  0.000163495693186191  0.000161472188193192  0.000159473727116499  0.000157500000000000  0.000156147543092883  0.000154806699771072  0.000153477370308384  0.000152159455834989  0.000150852858330055  0.000149557480614456  0.000148273226343548  0.000147000000000000  0.000145711134230601  0.000144433568971212  0.000143167205141286  0.000141911944528993  0.000140667689783603  0.000139434344407940  0.000138211812750893  0.000137000000000000  0.000135303243264230  0.000133627501005982  0.000131972512959150  0.000130338022081049  0.000128723774512488  0.000127129519538343  0.000125555009548622  0.000124000000000000  0.000121735416714837  0.000119512190989798  0.000117329567521341  0.000115186804799855  0.000113083174857750  0.000111017963022137  0.000108990467672034  0.000107000000000000  0.000105351271499624  0.000103727947725117  0.000102129637223249  0.000100555954572566  9.90065202904512e-05  9.74809607416110e-05  9.59789080479782e-05  9.45000000000000e-05  9.49305707329649e-05  9.53631032771054e-05  9.57976065710296e-05  9.62340895940726e-05  9.66725613664824e-05  9.71130309496059e-05  9.75555074460760e-05  9.80000000000000e-05  0.000101765319635228  0.000105675308984286  0.000109735526493238  0.000113951744172698  0.000118329955803329  0.000122876385456614  0.000127597496342998  0.000132500000000000  0.000139886420567755  0.000147684608749123  0.000155917519176334  0.000164609386123635  0.000173785794842949  0.000183473756876245  0.000193701789566313  0.000204500000000000  0.000214978054456681  0.000225992977496234  0.000237572276885149  0.000249744869817180  0.000262541155128775  0.000275993089214640  0.000290134265833019  0.000305000000000000  0.000316970154486588  0.000329410094541809  0.000342338257561832  0.000355773804544404  0.000369736648487610  0.000384247483903184  0.000399327817488115  0.000415000000000000  0.000426969448044210  0.000439284119429343  0.000451953971102009  0.000464989247187502  0.000478400487272628  0.000492198534927410  0.000506394546472590  0.000521000000000000  0.000532561120764876  0.000544378785701230  0.000556458687588618  0.000568806645530799  0.000581428607758910  0.000594330654496827  0.000607519000890119  0.000621000000000000  0.000631372889140177  0.000641919042095354  0.000652641352982027  0.000663542764258642  0.000674626267533073  0.000685894904383592  0.000697351767193550  0.000709000000000000  0.000718082202343384  0.000727280746575918  0.000736597123022816  0.000746032841100175  0.000755589429559530  0.000765268436735528  0.000775071430796794  0.000785000000000000  0.000794350730229612  0.000803812844097221  0.000813387668377794  0.000823076545650525  0.000832880834487084  0.000842801909642120  0.000852841162246024  0.000863000000000000  0.000871978658514245  0.000881050731059447  0.000890217189515887  0.000899479015875301  0.000908837202346070  0.000918292751459519  0.000927846676177320  0.000937500000000000  0.000943668557671521  0.000949877703187038  0.000956127703606238  0.000962418827746008  0.000968751346191991  0.000975125531310229  0.000981541657258879  0.000988000000000000  0.000990232267613204  0.000992469578767599  0.000994711944858463  0.000996959377306819  0.000999211887559494  0.00100146948708918  0.00100373218739448  0.00100600000000000  0.00100524803566562  0.00100449663340913  0.00100374579281040  0.00100299551344959  0.00100224579490718  0.00100149663676399  0.00100074803860112  0.00100000000000000  0.000998365682010934  0.000996734035017158  0.000995105054653415  0.000993478736561584  0.000991855076390666  0.000990234069796774  0.000988615712443119  0.000987000000000000  0.000985870486221567  0.000984742265048378  0.000983615335001187  0.000982489694602442  0.000981365342376280  0.000980242276848529  0.000979120496546703  0.000978000000000000  0.000977749776042945  0.000977499616106369  0.000977249520173893  0.000976999488229140  0.000976749520255739  0.000976499616237324  0.000976249776157530  0.000976000000000000  0.000977306363660315  0.000978614475871872  0.000979924338975085  0.000981235955313502  0.000982549327233805  0.000983864457085817  0.000985181347222510  0.000986500000000000  0.000989223537113762  0.000991954593390636  0.000994693189589588  0.000997439346526896  0.00100019308507631  0.00100295442616919  0.00100572339079472  0.00100850000000000  0.00101177503320670  0.00101506070185466  0.00101835704048172  0.00102166408373790  0.00102498186638573  0.00102831042330063  0.00103164978947129  0.00103500000000000  0.00103876423383917  0.00104254215797438  0.00104633382219642  0.00105013927647717  0.00105395857097027  0.00105779175601175  0.00106163888212072  0.00106550000000000  0.00107023809720152  0.00107499726391510  0.00107977759383326  0.00108457918106517  0.00108940212013850  0.00109424650600125  0.00109911243402365  0.00110400000000000  0.00110964783272621  0.00111532455858150  0.00112103032537706  0.00112676528168026  0.00113252957681851  0.00113832336088315  0.00114414678473333  0.00115000000000000  0.00115697511797729  0.00116399254227702  0.00117105252950056  0.00117815533780567  0.00118530122691591  0.00119249045813015  0.00119972329433211  0.00120700000000000  0.00121511877143291  0.00122329215301460  0.00123152051207507  0.00123980421841515  0.00124814364432308  0.00125653916459126  0.00126499115653311  0.00127350000000000  0.00128329473854583  0.00129316481034897  0.00130311079481229  0.00131313327579496  0.00132323284164674  0.00133341008524249  0.00134366560401698  0.00135400000000000  0.00136570232244174  0.00137750578546733  0.00138941126321245  0.00140141963736777  0.00141353179724418  0.00142574863983871  0.00143807106990095  0.00145050000000000  0.00146445822048664  0.00147855076149665  0.00149277891560190  0.00150714398781271  0.00152164729569758  0.00153629016950403  0.00155107395228058  0.00156600000000000  0.00158215454507561  0.00159847573723079  0.00161496529556372  0.00163162495690646  0.00164845647600789  0.00166546162571852  0.00168264219717725  0.00170000000000000  0.00171806374070662  0.00173631942184167  0.00175476908291227  0.00177341478509682  0.00179225861147522  0.00181130266726164  0.00183054908003966  0.00185000000000000  0.00186997834947776  0.00189017244730572  0.00191058462337357  0.00193121723273173  0.00195207265586308  0.00197315329895758  0.00199446159418989  0.00201600000000000  0.00203813077588541  0.00206050449385479  0.00208312382082125  0.00210599145297411  0.00212911011610028  0.00215248256590920  0.00217611158836128  0.00220000000000000  0.00222423243503663  0.00224873178412226  0.00227350098724660  0.00229854301678259  0.00232386087784308  0.00234945760864149  0.00237533628085636  0.00240150000000000  0.00242789915924855  0.00245458851862579  0.00248157126823764  0.00250885063325819  0.00253642987431511  0.00256431228787948  0.00259250120665972  0.00262100000000000  0.00264957317308165  0.00267845784033345  0.00270765739754969  0.00273717527754435  0.00276701495055469  0.00279717992464920  0.00282767374614006  0.00285850000000000  0.00288942809599410  0.00292069082453038  0.00295229180623063  0.00298423470089067  0.00301652320790416  0.00304916106669111  0.00308215205713089  0.00311550000000000  0.00314913847838094  0.00318314015600046  0.00321750895437027  0.00325224883734317  0.00328736381157023  0.00332285792696288  0.00335873527715999  0.00339500000000000  0.00343153165238261  0.00346845640097312  0.00350577847565489  0.00354350215182663  0.00358163175089220  0.00362017164075556  0.00365912623632124  0.00369850000000000  0.00373775999614775  0.00377743674159861  0.00381753466019359  0.00385805822273330  0.00389901194747639  0.00394040040064334  0.00398222819692557  0.00402450000000000  0.00406771546525913  0.00411139498231042  0.00415554353418633  0.00420016615742759  0.00424526794265777  0.00429085403516407  0.00433692963548427  0.00438350000000000  0.00442917416568330  0.00447532423633087  0.00452195517066568  0.00456907197907847  0.00461667972416603  0.00466478352127526  0.00471338853905271  0.00476250000000000  0.00480824474447527  0.00485442887617302  0.00490105661549352  0.00494813222337479  0.00499566000168201  0.00504364429360060  0.00509208948403312  0.00514100000000000  0.00518585601885603  0.00523110341340309  0.00527674559845453  0.00532278601861845  0.00536922814855771  0.00541607549325212  0.00546333158826302  0.00551100000000000  0.00555665411528014  0.00560268643746320  0.00564910009968321  0.00569589826102960  0.00574308410676232  0.00579066084852854  0.00583863172458132  0.00588700000000000  0.00593675317251640  0.00598692682714175  0.00603752451752291  0.00608854982733984  0.00614000637055953  0.00619189779169190  0.00624422776604793  0.00629700000000000  0.00635285956575791  0.00640921465177732  0.00646606965373024  0.00652342900628189  0.00658129718343665  0.00663967869888695  0.00669857810636541  0.00675800000000000  0.00681997394317172  0.00688251621567643  0.00694563202935337  0.00700932664383677  0.00707360536699409  0.00713847355536835  0.00720393661462453  0.00727000000000000  0.00733880237029074  0.00740825587760454  0.00747836668422070  0.00754914101073758  0.00762058513662448  0.00769270540077883  0.00776550820208860  0.00783900000000000  0.00791534623463626  0.00799243602681087  0.00807027661824068  0.00814887532117163  0.00822823951906572  0.00830837666729458  0.00838929429383976  0.00847100000000000  0.00855700588960946  0.00864388499525570  0.00873164618268325  0.00882029840765040  0.00890985071684304  0.00900031224879789  0.00909169223483504  0.00918400000000000  0.00927869849578310  0.00937437345118113  0.00947103493471073  0.00956869311870749  0.00966735828039647  0.00976704080297376  0.00986775117669912  0.00996950000000000  0.0100703557740612  0.0101722318487555  0.0102751385458854  0.0103790862916733  0.0104840856178175  0.0105901471625599  0.0106972816717632  0.0108055000000000  0.0109118275667330  0.0110192014109588  0.0111276318281878  0.0112371292152400  0.0113477040712412  0.0114593669986302  0.0115721287041751  0.0116860000000000  0.0118009044183696  0.0119169386523614  0.0120341138110638  0.0121524411127970  0.0122719318861872  0.0123925975712509  0.0125144497204904  0.0126375000000000  0.0127668919890193  0.0128976087880740  0.0130296639615404  0.0131630712126768  0.0132978443850456  0.0134339974639496  0.0135715445778836  0.0137105000000000  0.0138570835261734  0.0140052342256916  0.0141549688537275  0.0143063043445888  0.0144592578136336  0.0146138465592061  0.0147700880645929  0.0149280000000000  0.0150908913353734  0.0152555601082562  0.0154220257136891  0.0155903077583478  0.0157604260628521  0.0159324006641002  0.0161062518176290  0.0162820000000000  0.0164627560022848  0.0166455186826413  0.0168303103183759  0.0170171534341088  0.0172060708045189  0.0173970854571202  0.0175902206750686  0.0177855000000000  0.0179862617310665  0.0181892896493452  0.0183946093354307  0.0186022466586700  0.0188122277804221  0.0190245791573536  0.0192393275447727  0.0194565000000000  0.0196861021421066  0.0199184137717191  0.0201534668629406  0.0203912937671939  0.0206319272176743  0.0208754003338547  0.0211217466260442  0.0213710000000000  0.0216272929443274  0.0218866594965023  0.0221491365169435  0.0224147613081202  0.0226835716198535  0.0229556056546816  0.0232309020732889  0.0235095000000000  0.0237835540544652  0.0240608028014916  0.0243412834821286  0.0246250337715505  0.0249120917841171  0.0252024960784936  0.0254962856628299  0.0257935000000000  0.0260837454707169  0.0263772569748636  0.0266740712640020  0.0269742255032466  0.0272777572759186  0.0275847045882511  0.0278951058741489  0.0282090000000000  0.0285243792697264  0.0288432845093126  0.0291657551394328  0.0294918310214880  0.0298215524625335  0.0301549602202613  0.0304920955080381  0.0308330000000000  0.0311959975551335  0.0315632686880905  0.0319348637117923  0.0323108335314953  0.0326912296517644  0.0330761041835290  0.0334655098512215  0.0338595000000000  0.0342742169296514  0.0346940133829738  0.0351189515748393  0.0355490944821384  0.0359845058531131  0.0364252502168050  0.0368713928926181  0.0373230000000000  0.0377766037992581  0.0382357204567189  0.0387004169727153  0.0391707611618666  0.0396468216629746  0.0401286679490403  0.0406163703374027  0.0411100000000000  0.0416032218122255  0.0421023611081789  0.0426074888835315  0.0431186769857332  0.0436359981242316  0.0441595258808140  0.0446893347200733  0.0452255000000000  0.0457715466649822  0.0463241862246884  0.0468834982808515  0.0474495633963053  0.0480224631065886  0.0486022799316891  0.0491890973879301  0.0497830000000000  0.0504080793241280  0.0510410071941744  0.0516818821571085  0.0523308039972634  0.0529878737518721  0.0536531937267994  0.0543268675124703  0.0550090000000000  0.0557219906966361  0.0564442227125743  0.0571758158277541  0.0579168913746240  0.0586675722582648  0.0594279829767721  0.0601982496419048  0.0609785000000000  0.0617708847904797  0.0625735662208601  0.0633866780907730  0.0642103559385089  0.0650447370636097  0.0658899605497557  0.0667461672879493  0.0676135000000000  0.0684894253212580  0.0693766981569682  0.0702754655129099  0.0711858762993053  0.0721080813554923  0.0730422334749153  0.0739884874304407  0.0749470000000000  0.0759203392222766  0.0769063192339325  0.0779051042013269  0.0789168604228527  0.0799417563566256  0.0809799626485329  0.0820316521606455  0.0830970000000000  0.0841842596969737  0.0852857453425215  0.0864016430721301  0.0875321414567244  0.0886774315345333  0.0898377068433729  0.0910131634533509  0.0922040000000000  0.0934194216459591  0.0946508648287004  0.0958985407421266  0.0971626633640721  0.0984434494930007  0.0997411187851862  0.101055893792384  0.102388000000000  0.103742075514020  0.105114058600193  0.106504186085149  0.107912697927538  0.109339837259446  0.110785850428371  0.112250987039741  0.113735500000000  0.115234613746229  0.116753486864195  0.118292379796289  0.119851556417720  0.121431284081754  0.123031833665562  0.124653479616667  0.126296500000000  0.127943062724790  0.129611092147444  0.131300868135293  0.133012674204378  0.134746797567018  0.136503529179998  0.138283163793389  0.140086000000000  0.141880396203356  0.143697777271258  0.145538437622516  0.147402675447225  0.149290792755072  0.151203095424260  0.153139893251059  0.155101500000000  0.157042508745560  0.159007808132734  0.160997702145352  0.163012498571429  0.165052509050777  0.167118049123209  0.169209438277337  0.171327000000000  0.173411939621883  0.175522251620723  0.177658244762062  0.179820231568920  0.182008528367523  0.184223455333582  0.186465336539143  0.188734500000000  0.190959899307533  0.193211538661681  0.195489727463119  0.197794778760715  0.200127009294547  0.202486739539429  0.204874293748943  0.207290000000000  0.209651074112598  0.212039041326480  0.214454207959391  0.216896883818095  0.219367382238115  0.221866020123931  0.224393117989626  0.226949000000000  0.229322352754063  0.231720525195788  0.234143776880732  0.236592370078792  0.239066569802592  0.241566643836165  0.244092862763934  0.246645500000000  0.248993274868951  0.251363397791425  0.253756081494589  0.256171540730523  0.258609992295492  0.261071655049408  0.263556749935471  0.266065500000000  0.268346731391538  0.270647521939229  0.272968039342236  0.275308452737561  0.277668932712382  0.280049651316478  0.282450782074777  0.284872500000000  0.287043987016511  0.289232026546384  0.291436744763848  0.293658268804916  0.295896726774712  0.298152247754868  0.300424961810957  0.302715000000000  0.304732673204297  0.306763794718587  0.308808454179308  0.310866741820350  0.312938748477034  0.315024565590122  0.317124285209852  0.319238000000000  0.321366713666017  0.323509621826023  0.325666819130445  0.327838400860851  0.330024462934156  0.332225101906859  0.334440414979310  0.336670500000000  0.338916358763153  0.341177199182210  0.343453121196676  0.345744225412732  0.348050613107682  0.350372386234429  0.352709647425984  0.355062500000000  0.357432033733673  0.359817380711817  0.362218646465195  0.364635937228834  0.367069359946731  0.369519022276578  0.371985032594531  0.374467500000000  0.376967604688507  0.379484401141863  0.382018000801407  0.384568515852507  0.387136059229530  0.389720744620839  0.392322686473830  0.394942000000000  0.397579878543000  0.400235375883715  0.402908609700815  0.405599698458961  0.408308761414058  0.411035918618541  0.413781290926693  0.416545000000000  0.419328401214170  0.422130401432814  0.424951124936604  0.427790696836666  0.430649243080134  0.433526890455737  0.436423766599418  0.439340000000000  0.442276921037274  0.445233474944715  0.448209792965072  0.451206007218432  0.454222250708085  0.457258657326428  0.460315361860906  0.463392500000000  0.466491544573823  0.469611314725359  0.472751949061706  0.475913587116926  0.479096369358251  0.482300437192318  0.485525932971454  0.488773000000000  0.492043140541659  0.495335159990627  0.498649204727948  0.501985422114029  0.505343960495194  0.508724969210278  0.512128598597271  0.515555000000000  0.519005757073783  0.522479611051645  0.525976716527770  0.529497229131088  0.533041305532195  0.536609103450329  0.540200781660390  0.543816500000000  0.547458002638862  0.551123889498263  0.554814323859758  0.558529470098266  0.562269493689395  0.566034561216808  0.569824840379647  0.573640500000000  0.577483307530537  0.581351857960532  0.585246323741212  0.589166878479052  0.593113696943512  0.597086955074831  0.601086829991869  0.605113500000000  0.609168900817158  0.613251480462400  0.617361421085094  0.621498906055353  0.625664119972212  0.629857248671873  0.634078479235985  0.638328000000000  0.642607714262073  0.646916122164665  0.651253416086275  0.655619789695217  0.660015437958266  0.664440557149370  0.668895344858404  0.673380000000000  0.677896657325465  0.682443609868185  0.687021060831575  0.691629214782025  0.696268277658039  0.700938456779446  0.705639960856658  0.710373000000000  0.715139842398363  0.719938671916803  0.724769703199684  0.729633152331704  0.734529236847565  0.739458175741698  0.744420189478061  0.749415500000000  0.754303270156316  0.759222918886136  0.764174654105013  0.769158685084554  0.774175222461253  0.779224478245402  0.784306665830045  0.789422000000000  0.794251322588469  0.799110188762832  0.803998779257229  0.808917275911449  0.813865861677696  0.818844720627388  0.823854037958010  0.828894000000000  0.833964597192257  0.839066212772734  0.844199036490366  0.849363259254837  0.854559073143686  0.859786671409447  0.865046248486840  0.870338000000000  0.875662222782577  0.881019016070220  0.886408579110377  0.891831112369377  0.897286817539883  0.902775897548399  0.908298556562814  0.913855000000000

    )
end

to init-age-USA2007
  let census-dist (list 0.069 0.067 0.069 0.071 0.069 0.07 0.065 0.07 0.074 0.076 0.07 0.061 0.047 0.036 0.028 0.025 0.019 0.013)
   
    ask turtles [
    let temp-init random 18
    while [random-float 1 > item temp-init census-dist][set temp-init random 18]
    set age (temp-init * 5) + random 5
    if timescale = "months" [ set age age * 8 + random 8]
    ]
    
end

to replacement  
  ask turtles [
       
     let x age  
     if x >= length life-distribution [set x length life-distribution - 1]
     
     ifelse  random-float 1  < item x life-distribution [replace][set age age + 1]
  ]
end

to replace  
    ifelse random-float 1.0 < 0.5 [set cooperate? true][set cooperate? false]        
    set age 0
    set rule? false
    set behavior? false
    set rewire? false
    if random-init
    [
    ;set theta_1 random-float 1.0
    ;set theta_2 random-float 1.0
    set weighting-history random-float 1.0
    set likelihood-to-rewire random-float 0.01
    ]
    set rule (random 4) + 1
    ;move-to one-of patches with [not any? turtles-here]
end

















;; do-calculations reports true if the network is connected,
;;   and reports false if the network is disconnected.
;; (In the disconnected case, the average path length does not make sense,
;;   or perhaps may be considered infinite)
to-report do-calculations

  ;; set up a variable so we can report if the network is disconnected
  let connected? network:mean-link-path-length turtles links
  ;set average-path-length network:mean-link-path-length turtles with [count link-neighbors > 0] links

  ;; find the path lengths in the network
  ;find-path-lengths

  ;let num-connected-pairs sum [length remove infinity (remove 0 distance-from-other-turtles)] of turtles

  ;; In a connected network on N nodes, we should have N(N-1) measurements of distances between pairs,
  ;; and none of those distances should be infinity.
  ;; If there were any "infinity" length paths between nodes, then the network is disconnected.
  ;; In that case, calculating the average-path-length doesn't really make sense.
  ;ifelse ( num-connected-pairs != (count turtles * (count turtles - 1) ))
  ifelse ( connected? = false)
  [
      ;set average-path-length infinity
      ;; report that the network is not connected
      ;set connected? false
  ]
  [
    ;set average-path-length (sum [sum distance-from-other-turtles] of turtles) / (num-connected-pairs)
    set connected? true
    
  ]
  ;; find the clustering coefficient and add to the aggregate for all iterations
  find-clustering-coefficient

  ;; report whether the network is connected or not
  report connected?
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clustering computations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report in-neighborhood? [ hood ]
  report ( member? end1 hood and member? end2 hood )
end


to find-clustering-coefficient
  ifelse all? turtles [count link-neighbors <= 1]
  [
    ;; it is undefined
    ;; what should this be?
    set clustering-coefficient 0
  ]
  [
    let total 0
    ask turtles with [ count link-neighbors <= 1]
      [ set node-clustering-coefficient "undefined" ]
    ask turtles with [ count link-neighbors > 1]
    [
      let hood link-neighbors
      set node-clustering-coefficient (2 * count links with [ in-neighborhood? hood ] /
                                         ((count hood) * (count hood - 1)) )
      ;; find the sum for the value at turtles
      set total total + node-clustering-coefficient
    ]
    ;; take the average
    set clustering-coefficient total / count turtles with [count link-neighbors > 1]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Path length computations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Implements the Floyd Warshall algorithm for All Pairs Shortest Paths
;; It is a dynamic programming algorithm which builds bigger solutions
;; from the solutions of smaller subproblems using memoization that
;; is storing the results.
;; It keeps finding incrementally if there is shorter path through
;; the kth node.
;; Since it iterates over all turtles through k,
;; so at the end we get the shortest possible path for each i and j.

to find-path-lengths
  ;; reset the distance list
  ask turtles
  [
    set distance-from-other-turtles []
  ]

  let i 0
  let j 0
  let k 0
  let node1 one-of turtles
  let node2 one-of turtles
  let node-count count turtles
  ;; initialize the distance lists
  while [i < node-count]
  [
    set j 0
    while [j < node-count]
    [
      set node1 turtle i
      set node2 turtle j
      ;; zero from a node to itself
      ifelse i = j
      [
        ask node1 [
          set distance-from-other-turtles lput 0 distance-from-other-turtles
        ]
      ]
      [
        ;; 1 from a node to it's neighbor
        ifelse [ link-neighbor? node1 ] of node2
        [
          ask node1 [
            set distance-from-other-turtles lput 1 distance-from-other-turtles
          ]
        ]
        ;; infinite to everyone else
        [
          ask node1 [
            set distance-from-other-turtles lput infinity distance-from-other-turtles
          ]
        ]
      ]
      set j j + 1
    ]
    set i i + 1
  ]
  set i 0
  set j 0
  let dummy 0
  while [k < node-count]
  [
    set i 0
    while [i < node-count]
    [
      set j 0
      while [j < node-count]
      [
        ;; alternate path length through kth node
        set dummy ( (item k [distance-from-other-turtles] of turtle i) +
                    (item j [distance-from-other-turtles] of turtle k))
        ;; is the alternate path shorter?
        if dummy < (item j [distance-from-other-turtles] of turtle i)
        [
          ask turtle i [
            set distance-from-other-turtles replace-item j distance-from-other-turtles dummy
          ]
        ]
        set j j + 1
      ]
      set i i + 1
    ]
    set k k + 1
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;
;;; Edge Operations ;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; creates a new lattice
to wire-them
  ;; iterate over the turtles
  let n 0
  while [n < count turtles]
  [
    ;; make edges with the next three neighbors
    ;; this makes a lattice with average degree of 6
    make-edge turtle n
              turtle ((n + 1) mod count turtles)
    make-edge turtle n
              turtle ((n + 2) mod count turtles)
    make-edge turtle n
              turtle ((n + 3) mod count turtles)
    make-edge turtle n
             turtle ((n + 4) mod count turtles)
;             make-edge turtle n
;             turtle ((n + 5) mod count turtles)
;             make-edge turtle n
;             turtle ((n + 6) mod count turtles)
    set n n + 1
  ]
end

;; connects the two turtles
to make-edge [node1 node2]
  ask node1 [ create-link-with node2  [
    set rewired? false
  ] ]
end

;;;;;;;;;;;;;;;;
;;; Graphics ;;;
;;;;;;;;;;;;;;;;

to prepare-highlight
    find-path-lengths
end


to highlight
  ;; remove any previous highlights

  ask turtles [ establish-color 
    ]
  ask links [ set color gray + 2 ]
  if mouse-inside? [ do-highlight ]
  display
  
end

to do-highlight
  ;; getting the node closest to the mouse
  let min-d min [distancexy mouse-xcor mouse-ycor] of turtles
  let node one-of turtles with [count link-neighbors > 0 and distancexy mouse-xcor mouse-ycor = min-d]
  if node != nobody
  [
    
    ;; highlight the chosen node
    ask node
    [
      set color pink - 1
      
      let pairs (length remove infinity distance-from-other-turtles)
      let local-val (sum remove infinity distance-from-other-turtles) / pairs
      ;; show node's clustering coefficient
      set highlight-string (word "clustering coefficient = " precision node-clustering-coefficient 3
                                 " and avg path length = " precision local-val 3
                                 " (for " pairs " turtles )")
    ]
    let neighbor-nodes [ link-neighbors ] of node
    let direct-links [ my-links ] of node
    ;; highlight neighbors
    ask neighbor-nodes
    [
      ;set color yellow

      set size 1.5
      ;; highlight edges connecting the chosen node to its neighbors
      ask my-links [
        ifelse (end1 = node or end2 = node)
        [
          set color blue - 1 ;
        ]
        [
          if (member? end1 neighbor-nodes and member? end2 neighbor-nodes)
            [ set color yellow ]
        ]
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;
;;; Plotting ;;;
;;;;;;;;;;;;;;;;

to do-plotting


  
     ;; plot the rewire-all graph
     set-current-plot "Network Properties Rewire-All"
     set-current-plot-pen "apl"
     ;; note: dividing by value at initial value to normalize the plot
     plotxy ticks 
            average-path-length / average-path-length-of-lattice

     set-current-plot-pen "cc"
     ;; note: dividing by initial value to normalize the plot
     plotxy ticks
            clustering-coefficient / clustering-coefficient-of-lattice
   
end



to change-layout
  ;layout-circle (sort turtles) max-pxcor - 1
  ;layout-tutte turtles with [changed-neighborhood? = true] links (max-pxcor - 1)
  ;layout-tutte turtles links (max-pxcor - 1)
  ;layout-spring turtles with [changed-neighborhood? = true] links 10 10 0.1
  layout-spring turtles links 1 5 10
  
end



; Copyright 2005 Uri Wilensky. All rights reserved.
; The full copyright notice is in the Information tab.
@#$#@#$#@
GRAPHICS-WINDOW
429
10
927
529
30
30
8.0
1
10
1
1
1
0
0
0
1
-30
30
-30
30
1
1
1
ticks
30.0

SLIDER
3
50
126
83
num_nodes
num_nodes
10
400
227
1
1
NIL
HORIZONTAL

SLIDER
4
526
177
559
rewiring_probability
rewiring_probability
0
1
1
0.01
1
NIL
HORIZONTAL

MONITOR
5
96
133
141
NIL
clustering-coefficient
3
1
11

MONITOR
139
96
267
141
NIL
average-path-length
3
1
11

PLOT
5
144
268
323
Network Properties Rewire-All
NIL
NIL
0.0
1.0
0.0
1.0
true
true
"" ""
PENS
"apl" 1.0 0 -2674135 true "" ""
"cc" 1.0 0 -10899396 true "" ""

BUTTON
228
10
283
43
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
286
10
341
43
NIL
highlight
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
129
50
240
83
inicoop
inicoop
0
100
50
1
1
NIL
HORIZONTAL

SLIDER
242
50
414
83
strength_of_dilemma
strength_of_dilemma
0
0.5
0.5
0.01
1
NIL
HORIZONTAL

BUTTON
345
10
408
43
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
952
19
1321
231
cooperation
time
NIL
0.0
100.0
0.0
1.0
true
true
"" ""
PENS
"cooperation" 1.0 0 -13791810 true "" ""
"fraction-best-maxi" 1.0 0 -2674135 true "" ""
"fraction-best-mini" 1.0 0 -10899396 true "" ""
"fraction-best-conf" 1.0 0 -13345367 true "" ""
"fraction-best-anti" 1.0 0 -16777216 true "" ""

PLOT
951
236
1321
442
population
time
fraction
0.0
100.0
0.0
1.0
true
true
"" ""
PENS
"maxi" 1.0 0 -2674135 true "" ""
"mini" 1.0 0 -10899396 true "" ""
"conf" 1.0 0 -13345367 true "" ""
"anti" 1.0 0 -16777216 true "" ""

SLIDER
4
364
220
397
Initial-likelihood-to-rewire
Initial-likelihood-to-rewire
0
1
0.502
0.001
1
NIL
HORIZONTAL

SLIDER
0
10
170
43
Transcription-error
Transcription-error
0
1
0.05
0.01
1
NIL
HORIZONTAL

BUTTON
170
10
225
43
NIL
prepare-highlight
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
5
328
153
361
Maturing-period
Maturing-period
1
1
-1000

SWITCH
153
328
276
361
Random-init
Random-init
1
1
-1000

SLIDER
4
399
198
432
initial-weighting-history
initial-weighting-history
0
1
0.5
0.01
1
NIL
HORIZONTAL

CHOOSER
222
364
360
409
timescale
timescale
"years" "months"
0

SWITCH
278
328
417
361
replacement?
replacement?
0
1
-1000

SWITCH
226
422
358
455
pref-rewiring?
pref-rewiring?
0
1
-1000

@#$#@#$#@
## WHAT IS IT?

This model explores the formation of networks that result in the "small world" phenomenon -- the idea that a person is only a couple of connections away any other person in the world.

A popular example of the small world phenomenon is the network formed by actors appearing in the same movie (e.g. the "six degrees of Kevin Bacon" game), but small worlds are not limited to people-only networks.  Other examples range from power grids to the neural networks of worms.  This model illustrates some general, theoretical conditions under which small world networks between people or things might occur.

## HOW IT WORKS

This model is an adaptation of a model proposed by Duncan Watts and Steve Strogatz (1998). It begins with a network where each person (or "node") is connected to his or her two neighbors on either side.  The REWIRE-ONE button picks a random connection (or "edge") and rewires it. By rewiring, we mean changing one end of a connected pair of nodes, and keeping the other end the same.

The REWIRE-ALL button creates the network and then visits all edges and tries to rewire them.  The rewiring_probability slider determines the probability that an edge will get rewired.  Running REWIRE-ALL at multiple probabilities produces a range of possible networks with varying average path lengths and clustering coefficients.

To identify small worlds, the "average path length" (abbreviated "apl") and "clustering coefficient" (abbreviated "cc") of the network are calculated and plotted after the REWIRE-ONE or REWIRE-ALL buttons are pressed. These two plots are separated because the x-axis is slightly different.  The REWIRE-ONE x-axis is the fraction of edges rewired so far, whereas the REWIRE-ALL x-axis is the probability of rewiring.  Networks with short average path lengths and high clustering coefficients are considered small world networks. (Note: The plots for both the clustering coefficient and average path length are normalized by dividing by the values of the initial network. The monitors give the actual values.)

Average Path Length: Average path length is calculated by finding the shortest path between all pairs of nodes, adding them up, and then dividing by the total number of pairs. This shows us, on average, the number of steps it takes to get from one member of the network to another.

Clustering Coefficient:  Another property of small world networks is that from one person's perspective it seems unlikely that they could be only a few steps away from anybody else in the world.  This is because their friends more or less know all the same people they do. The clustering coefficient is a measure of this "all-my-friends-know-each-other" property.  This is sometimes described as the friends of my friends are my friends.  More precisely, the clustering coefficient of a node is the ratio of existing links connecting a node's neighbors to each other to the maximum possible number of such links.  You can see this is if you press the HIGHLIGHT button and click a node, that will display all of the neighbors in blue and the edges connecting those neighbors in yellow.  The more yellow links, the higher the clustering coefficient for the node you are examining (the one in pink) will be.  The clustering coefficient for the entire network is the average of the clustering coefficients of all the nodes. A high clustering coefficient for a network is another indication of a small world.


## HOW TO USE IT

The num_nodes slider controls the size of the network.  Choose a size and press SETUP.

Pressing the REWIRE-ONE button picks one edge at random, rewires it, and then plots the resulting network properties. The REWIRE-ONE button always rewires at least one edge (i.e., it ignores the rewiring_probability).

Pressing the REWIRE-ALL button re-creates the initial network (each node connected to its two neighbors on each side for a total of four neighbors) and rewires all the edges with the current rewiring probability, then plots the resulting network properties on the rewire-all plot. Changing the rewiring_probability slider changes the fraction of links rewired after each run.

When you press HIGHLIGHT and then point to node in the view it color-codes the nodes and edges.  The node itself turns pink. Its neighbors and the edges connecting the node to those neighbors turn blue. Edges connecting the neighbors of the node to each other turn yellow. The amount of yellow between neighbors can gives you an indication of the clustering coefficient for that node.  The NODE-PROPERTIES monitor displays the average path length and clustering coefficient of the highlighted node only.  The AVERAGE-PATH-LENGTH and CLUSTERING-COEFFICIENT monitors display the values for the entire network.

## THINGS TO NOTICE

Note that for certain ranges of the fraction of nodes, the average path length decreases faster than the clustering coefficient.  In fact, there is a range of values for which the average path length is much smaller than clustering coefficient.  (Note that the values for average path length and clustering coefficient have been normalized, so that they are more directly comparable.)  Networks in that range are considered small worlds.

## THINGS TO TRY

Try plotting the values for different rewiring probabilities and observe the trends of the values for average path length and clustering coefficient.  What is the relationship between rewiring probability and fraction of nodes?  In other words, what is the relationship between the rewire-one plot and the rewire-all plot?

Do the trends depend on the number of nodes in the network?

Can you get a small world by repeatedly pressing REWIRE-ONE?

Set num_nodes to 80 and then press SETUP. Go to BehaviorSpace and run the VARY-rewiring_probability experiment. Try running the experiment multiple times without clearing the plot (i.e., do not run SETUP again).  What range of rewiring probabilities result in small world networks?

## EXTENDING THE MODEL

Try to see if you can produce the same results if you start with a different initial network.  Create new BehaviorSpace experiments to compare results.

In a precursor to this model, Watts and Strogatz created an "alpha" model where the rewiring was not based on a global rewiring probability.  Instead, the probability that a node got connected to another node depended on how many mutual connections the two nodes had. The extent to which mutual connections mattered was determined by the parameter "alpha."  Create the "alpha" model and see if it also can result in small world formation.

## NETWORK CONCEPTS

In this model we need to find the shortest paths between all pairs of nodes.  This is accomplished through the use of a standard dynamic programming algorithm called the Floyd Warshall algorithm. You may have noticed that the model runs slowly for large number of nodes.  That is because the time it takes for the Floyd Warshall algorithm (or other "all-pairs-shortest-path" algorithm) to run grows polynomially with the number of nodes.  For more information on the Floyd Warshall algorithm please consult:  http://en.wikipedia.org/wiki/Floyd-Warshall_algorithm

## NETLOGO FEATURES

The various network/link features (introduced in NetLogo 4.0) are used extensively in this model.

Lists are used heavily in the procedures that calculates shortest paths.

## RELATED MODELS

See other models in the Networks section of the Models Library, such as Giant Component and Preferential Attachment.

## CREDITS AND REFERENCES

This model is adapted from:  
Duncan J. Watts, Six Degrees: The Science of a Connected Age (W.W. Norton & Company, New York, 2003), pages 83-100.

The work described here was originally published in:  
DJ Watts and SH Strogatz. Collective dynamics of 'small-world' networks, Nature,  
393:440-442 (1998)

For more information please see Watts' website:  http://smallworld.columbia.edu/index.html

The small worlds idea was first made popular by Stanley Milgram's famous experiment (1967) which found that two random US citizens where on average connected by six acquaintances (giving rise to the popular "six degrees of separation" expression):  
Stanley Milgram.  The Small World Problem,  Psychology Today,  2: 60-67 (1967).

This experiment was popularized into a game called "six degrees of Kevin Bacon" which you can find more information about here:  http://www.cs.virginia.edu/oracle/

## HOW TO CITE

If you mention this model in an academic publication, we ask that you include these citations for the model itself and for the NetLogo software:  
- Wilensky, U. (2005).  NetLogo Small Worlds model.  http://ccl.northwestern.edu/netlogo/models/SmallWorlds.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.  
- Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

In other publications, please use:  
- Copyright 2005 Uri Wilensky. All rights reserved. See http://ccl.northwestern.edu/netlogo/models/SmallWorlds for terms of use.

## COPYRIGHT NOTICE

Copyright 2005 Uri Wilensky. All rights reserved.

Permission to use, modify or redistribute this model is hereby granted, provided that both of the following requirements are followed:  
a) this copyright notice is included.  
b) this model will not be redistributed for profit without permission from Uri Wilensky. Contact Uri Wilensky for appropriate licenses for redistribution for profit.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0
@#$#@#$#@
setup
repeat 5 [rewire-one]
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="vary-rewiring_probability" repetitions="5" runMetricsEveryStep="false">
    <go>rewire-all</go>
    <timeLimit steps="1"/>
    <exitCondition>rewiring_probability &gt; 1</exitCondition>
    <metric>average-path-length</metric>
    <metric>clustering-coefficient</metric>
    <steppedValueSet variable="rewiring_probability" first="0" step="0.025" last="1"/>
  </experiment>
  <experiment name="inicoopsweep_lowp" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 100</exitCondition>
    <metric>cooperation-rate</metric>
    <steppedValueSet variable="num_nodes" first="60" step="20" last="100"/>
    <steppedValueSet variable="inicoop" first="0" step="20" last="100"/>
    <steppedValueSet variable="rewiring_probability" first="0" step="0.04" last="0.2"/>
    <steppedValueSet variable="strength_of_dilemma" first="0" step="0.2" last="0.4"/>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
