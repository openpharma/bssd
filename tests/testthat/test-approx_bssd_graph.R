test_that("approx_bssd_graph input parameters are of expected class.", {

  # create sample data for testing:
  # these parameters will be fed into the approx_bssd_graph() function
  # which will create a graph that represents all possible occurrences of
  # observed events for an ongoing trial given a fixed sample size
  time <- 200
  prob_arm1 <- 1 / 3
  pi1_alpha <- 130
  pi1_beta <- 1500
  pi2_alpha <- 0.3
  pi2_beta <- 0.3
  num_samples <- 5000

  # sample data is fed into the approx_bssd_graph() function:
  approx_bssd_graph <- approx_bssd_graph(time = time,
                                         prob_arm1 = prob_arm1,
                                         pi1_alpha = pi1_alpha,
                                         pi1_beta = pi1_beta,
                                         pi2_alpha = pi2_alpha,
                                         pi2_beta = pi2_beta,
                                         num_samples = num_samples)

  # expect input parameters are of 'numeric' class
  expect_equal(class(c(time, prob_arm1, pi1_alpha,
                       pi1_beta, pi2_alpha, pi2_beta, num_samples)),
               "numeric")
})


test_that("approx_bssd_graph output is of expected class.", {

  # sample data is fed into the approx_bssd_graph() function:
  approx_bssd_graph <- approx_bssd_graph(time = 200,
                                         prob_arm1 = 1 / 3,
                                         pi1_alpha = 130,
                                         pi1_beta = 1500,
                                         pi2_alpha = 0.3,
                                         pi2_beta = 0.3,
                                         num_samples = 5000)

  # expect output plot is of class 'ggplot'
  expect_s3_class(approx_bssd_graph, "ggplot")
})


test_that("approx_bssd_graph output has expected attributes.", {

  # sample data is fed into the approx_bssd_graph() function:
  set.seed(202406)
  approx_bssd_graph <- approx_bssd_graph(time = 200,
                                         prob_arm1 = 1 / 3,
                                         pi1_alpha = 130,
                                         pi1_beta = 1500,
                                         pi2_alpha = 0.3,
                                         pi2_beta = 0.3,
                                         num_samples = 5000)

  # expect plot title is 'Risk levels as a function of effective sample size
  # and number of observed AEs'
  expect_identical(approx_bssd_graph$labels$title,
                   paste0("Risk levels as a function of effective sample ",
                          "size\nand number of observed AEs"))

  # expect plot x-axis label is 'Effective Sample Size'
  expect_identical(approx_bssd_graph$labels$x,
                   "Effective Sample Size")

  # expect plot y-axis label is 'Incidence of observed AEs'
  expect_identical(approx_bssd_graph$labels$y,
                   "Incidence of observed AEs")

  # expect no error when printing the plot
  expect_error(print(approx_bssd_graph), NA)

  # expect output data dimension names to be 'R', 'N', 'value', 'Signals'
  expect_equal(attr(approx_bssd_graph$data, "names"),
               c("R", "N", "value", "Signal"))

  # expect output data is of dimension 674x4
  expect_equal(dim(approx_bssd_graph$data), c(674, 4))
})


test_that("approx_bssd_graph stop condition works as expected.", {

  # expect error if time is not an integer
  expect_error(
    approx_bssd_graph(time = 200.75,
                      prob_arm1 = 1 / 3,
                      pi1_alpha = 130,
                      pi1_beta = 1500,
                      pi2_alpha = 0.3,
                      pi2_beta = 0.3,
                      num_samples = 5000),
    "time should be an integer. The x-axis in this graph is discontinuous.")
})


test_that("specific parameters results in expected approx_bssd_graph output", {

  # create sample data for testing:
  seed <- 202406
  set.seed(seed)
  time <- 100
  prob_arm1 <- 1 / 3
  pi1_alpha <- 150
  pi1_beta <- 1000
  pi2_alpha <- 0.3
  pi2_beta <- 0.3
  num_samples <- 5000

  approx_bssd_graph <- approx_bssd_graph(time = time,
                                         prob_arm1 = prob_arm1,
                                         pi1_alpha = pi1_alpha,
                                         pi1_beta = pi1_beta,
                                         pi2_alpha = pi2_alpha,
                                         pi2_beta = pi2_beta,
                                         num_samples = num_samples)

  # by using the above parameters we expect the following 'R', 'N', 'value' and
  # 'Signal' values
  if (all(seed == 202406 & time == 100 & prob_arm1 == 1 / 3 &
          pi1_alpha == 150 & pi1_beta == 1000 &
          pi2_alpha == 0.3 & pi2_beta == 0.3 &
          num_samples == 5000)) {

    expect_equal(nrow(approx_bssd_graph$data), 284)

    expect_equal(approx_bssd_graph$data$R,
                 c(0, 1, 2, 0, 1, 2, 3, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 6,
                   7, 0, 1, 2, 3, 4, 5, 6, 7, 8, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                   0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0, 1, 2, 3, 4, 5, 6, 7, 8,
                   9, 10, 11, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 0, 1, 2,
                   3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 0, 1, 2, 3, 4, 5, 6, 7,
                   8, 9, 10, 11, 12, 13, 14, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                   11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                   13, 14, 15, 16, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
                   14, 15, 16, 17, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
                   14, 15, 16, 17, 18, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                   13, 14, 15, 16, 17, 18, 19, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                   11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 0, 1, 2, 3, 4, 5, 6,
                   7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 0,
                   1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
                   18, 19, 20, 21, 22, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                   13, 14, 15, 16, 17, 18, 19, 20, 21, 22))

    expect_equal(approx_bssd_graph$data$N,
                 c(5, 5, 5, 10, 10, 10, 10, 15, 15, 15, 15, 15, 15, 20, 20, 20,
                   20, 20, 20, 20, 20, 25, 25, 25, 25, 25, 25, 25, 25, 25, 30,
                   30, 30, 30, 30, 30, 30, 30, 30, 30, 35, 35, 35, 35, 35, 35,
                   35, 35, 35, 35, 35, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
                   40, 40, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45,
                   50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 55,
                   55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 60,
                   60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60,
                   65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
                   65, 65, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70,
                   70, 70, 70, 70, 70, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75,
                   75, 75, 75, 75, 75, 75, 75, 75, 75, 80, 80, 80, 80, 80, 80,
                   80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 85,
                   85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85,
                   85, 85, 85, 85, 85, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90,
                   90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 95, 95, 95,
                   95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
                   95, 95, 95, 95, 95, 100, 100, 100, 100, 100, 100, 100, 100,
                   100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
                   100, 100, 100))

    expect_equal(approx_bssd_graph$data$value,
                 c(0.227502393677508, 0.547677182426908, 0.918861658475767,
                   0.0897037067223455, 0.275030433646096, 0.644305198546541,
                   0.900025062779258, 0.0483658990168055, 0.139841411954957,
                   0.398498321759482, 0.695969666054656, 0.908333281325544,
                   0.971404760632133, 0.0313032926306303, 0.0841990473274224,
                   0.236341220560215, 0.489257047596094, 0.737510977780072,
                   0.908506145469219, 0.968335915461265, 0.992270335652933,
                   0.0147469498294222, 0.0433192286621956, 0.135558717853525,
                   0.311816314841374, 0.56683174852205, 0.78874198137141,
                   0.910940483288345, 0.971250851044388, 0.991010507236469,
                   0.00923685481209716, 0.0259191507286828, 0.082063056257042,
                   0.212522740924579, 0.409282111828012, 0.641510207970833,
                   0.818010350014957, 0.926588404075659, 0.973459721673731,
                   0.990393125417521, 0.00624685978583318, 0.0160763879212119,
                   0.0480007527930538, 0.125555508635591, 0.28631743446676,
                   0.514224124670456, 0.695657125789357, 0.843524238497212,
                   0.927771260487921, 0.96904783039759, 0.990058546382651,
                   0.00283199609591499, 0.0101359896049991, 0.0260905480537233,
                   0.0715633730703145, 0.199290156729785, 0.368111380915607,
                   0.593228364648041, 0.76807751326862, 0.870320133066624,
                   0.93862677637466, 0.974006606393148, 0.990977786195968,
                   0.00207084856263791, 0.00568095123142402, 0.0142594093624803,
                   0.0501266237185348, 0.125308244189553, 0.241376462872641,
                   0.466483410206155, 0.642154173463732, 0.774359256032025,
                   0.894051267475397, 0.939140378478843, 0.975289896080651,
                   0.990089205714165, 0.00103809349216293, 0.00344061681596277,
                   0.0105232548887868, 0.0297850094935352, 0.077334386320006,
                   0.159701391644907, 0.311838369467797, 0.488262946882908,
                   0.676998102379194, 0.78396663898001, 0.899862032747259,
                   0.953765685794164, 0.974310763913105, 0.990271286696882,
                   0.000701523867916166, 0.0021923599741405,
                   0.00589795187025537, 0.0164913369648055, 0.0471419176740592,
                   0.112872270852032, 0.243544193656966, 0.404103647878492,
                   0.5721230060901, 0.724681092054297, 0.826054236760194,
                   0.912058250542858, 0.955260296010393, 0.978105745543563,
                   0.99053336169603, 0.000513498546646981, 0.00117724986552028,
                   0.00443575754214934, 0.00981523868053728, 0.0301210309577251,
                   0.0794259436719476, 0.170200747191465, 0.280811351577347,
                   0.475470985381915, 0.612769995758921, 0.787188350716391,
                   0.860989936048552, 0.913688135330891, 0.95984834384015,
                   0.979916341006917, 0.991492933423294, 0.0002681962091976,
                   0.000841798847143691, 0.00232228928532614,
                   0.00680303817916646, 0.0197508181047516, 0.0521196882174681,
                   0.118550348202746, 0.212185419532065, 0.365261417932457,
                   0.531419827367833, 0.651341345726002, 0.771500608544839,
                   0.874120946868564, 0.926351115440258, 0.963432209257328,
                   0.981010597258138, 0.992404144598156, 0.000145610420650854,
                   0.000544425415599671, 0.00141366934735641,
                   0.0040107697621283, 0.0110798203867518, 0.0328134729000724,
                   0.0800489241267067, 0.171255972727874, 0.262235158691202,
                   0.432232608610805, 0.546957213297424, 0.727864035601015,
                   0.804947631744382, 0.885822686150725, 0.935714853684636,
                   0.96792412221257, 0.983344237034347, 0.993429588373329,
                   9.8334402811971e-05, 0.00031860856609545,
                   0.000760201913601728, 0.00252530930858587,
                   0.00604755864572502, 0.0216271513388173, 0.0536446066588251,
                   0.111433194805836, 0.213969102589183, 0.362731994196587,
                   0.499349071154698, 0.644626690601668, 0.766725997104419,
                   0.81977225854893, 0.893387353063431, 0.941925179549946,
                   0.972190490871302, 0.982963899067527, 0.993378571774887,
                   6.02610178149533e-05, 0.000218924951021179,
                   0.000633968617793434, 0.00161654857114613,
                   0.00452142710778103, 0.012676719802173, 0.0327635116437758,
                   0.0785682654834391, 0.150577366807612, 0.269321397068197,
                   0.401853158440976, 0.537830719459912, 0.665890877478648,
                   0.771262874404312, 0.852477394970324, 0.89988826731277,
                   0.938974618197946, 0.968616131995629, 0.984298770548881,
                   0.992443409060203, 3.3113364535691e-05, 0.000121620393486342,
                   0.00029558392371256, 0.00079582872788352,
                   0.00293849859197776, 0.00802329707846248, 0.0215758906747655,
                   0.0506971716418671, 0.107634168350283, 0.191147627210406,
                   0.331550582693968, 0.430179135890457, 0.529565026617864,
                   0.696052599666664, 0.776248009277267, 0.86419291645314,
                   0.914769788595947, 0.954106743828415, 0.97312536532252,
                   0.985761905539445, 0.992803681400573, 2.47169838612369e-05,
                   7.09225888185307e-05, 0.000205870175497182,
                   0.000620765521251985, 0.00162172677619346,
                   0.0043399950621474, 0.0141781088564676, 0.0345208078844606,
                   0.0646577748629816, 0.148487147204221, 0.231856655172208,
                   0.355561083714698, 0.468593939944582, 0.632533924722127,
                   0.748696560870329, 0.800697631385596, 0.874812601727742,
                   0.921388687828201, 0.953682682822132, 0.974774227362225,
                   0.98919512624255, 0.993091379232277, 1.79375500353514e-05,
                   5.21683763749141e-05, 0.000146437518786043,
                   0.000404825807364838, 0.00130427221493061,
                   0.00344519806306229, 0.00954036685865291, 0.0212232067854463,
                   0.0558832500951565, 0.0877418387266281, 0.174144053917143,
                   0.26681533162878, 0.396544166991737, 0.515214871532232,
                   0.652052936551771, 0.741026853604967, 0.820856918722827,
                   0.893853177342122, 0.938250899667237, 0.963074554403908,
                   0.97703624888929, 0.989352304107775, 0.994599548781052,
                   1.79705389942993e-05, 3.82703940817249e-05,
                   7.3460029013608e-05, 0.00028951501667323,
                   0.000658201628821785, 0.00237445016905248,
                   0.00635982187273461, 0.0158739031955595, 0.0461346816740068,
                   0.0642206744115916, 0.138349891753977, 0.257260118004329,
                   0.332158620071679, 0.459184375849904, 0.569971811031619,
                   0.688661674460341, 0.762112566655322, 0.838749785253743,
                   0.903909059011052, 0.936924488065531, 0.964318052682495,
                   0.979671877079435, 0.990349118345997))

    expect_equal(as.character(approx_bssd_graph$data$Signal),
                 c("none", "none", "alert", "none", "none", "none", "alert",
                   "none", "none", "none", "none", "alert", "alert", "none",
                   "none", "none", "none", "none", "alert", "alert", "alert",
                   "none", "none", "none", "none", "none", "none", "alert",
                   "alert", "alert", "none", "none", "none", "none", "none",
                   "none", "warn", "alert", "alert", "alert", "none", "none",
                   "none", "none", "none", "none", "none", "warn", "alert",
                   "alert", "alert", "none", "none", "none", "none", "none",
                   "none", "none", "none", "warn", "alert", "alert", "alert",
                   "none", "none", "none", "none", "none", "none", "none",
                  "none", "none", "warn", "alert", "alert", "alert", "none",
                  "none", "none", "none", "none", "none", "none", "none",
                  "none", "none", "warn", "alert", "alert", "alert", "none",
                  "none", "none", "none", "none", "none", "none", "none",
                  "none", "none", "warn", "alert", "alert", "alert", "alert",
                  "none", "none", "none", "none", "none", "none", "none",
                  "none", "none", "none", "none", "warn", "alert", "alert",
                  "alert", "alert", "none", "none", "none", "none", "none",
                  "none", "none", "none", "none", "none", "none", "none",
                  "warn", "alert", "alert", "alert", "alert", "none", "none",
                  "none", "none", "none", "none", "none", "none", "none",
                  "none", "none", "none", "warn", "warn", "alert", "alert",
                  "alert", "alert", "none", "none", "none", "none", "none",
                  "none", "none", "none", "none", "none", "none", "none",
                  "none", "warn", "warn", "alert", "alert", "alert", "alert",
                  "none", "none", "none", "none", "none", "none", "none",
                  "none", "none", "none", "none", "none", "none", "none",
                  "warn", "warn", "alert", "alert", "alert", "alert", "none",
                  "none", "none", "none", "none", "none", "none", "none",
                  "none", "none", "none", "none", "none", "none", "none",
                  "warn", "alert", "alert", "alert", "alert", "alert", "none",
                  "none", "none", "none", "none", "none", "none", "none",
                  "none", "none", "none", "none", "none", "none", "none",
                  "warn", "warn", "alert", "alert", "alert", "alert", "alert",
                  "none", "none", "none", "none", "none", "none", "none",
                  "none", "none", "none", "none", "none", "none", "none",
                  "none", "none", "warn", "warn", "alert", "alert", "alert",
                  "alert", "alert", "none", "none", "none", "none", "none",
                  "none", "none", "none", "none", "none", "none", "none",
                  "none", "none", "none", "none", "none", "warn", "alert",
                  "alert", "alert", "alert", "alert"))

  }

})
