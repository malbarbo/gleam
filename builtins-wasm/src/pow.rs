// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
// SPDX-FileCopyrightText: 2018-2020 Arm Limited
// SPDX-FileCopyrightText: 2025 The Gleam contributors

//! `pow` of Arm's optimized-routines (`math/pow.c`), the one in glibc since
//! 2.28 and in musl, so that `float.power` gives the same result as on the
//! Erlang and JavaScript targets, which use the C library. The `libm` crate
//! has the older fdlibm one, whose result differs in the last bit in about 8%
//! of the cases with a fractional exponent.
//!
//! This is the variant without fused multiply-add, which WebAssembly lacks,
//! and that does not raise floating-point exceptions nor set errno. A C
//! library built with fused multiply-add rounds about 0.02% of the results
//! differently. Worst-case error: 0.54 ulp.

const N_LOG: usize = 1 << 7;
const N_EXP: usize = 1 << 7;
const OFF: u64 = 0x3fe6955500000000;
const SIGN_BIAS: u64 = 0x800 << 7;
// The top 12 bits of 2^-54, 512 and 1024.
const TOP12_TINY: u32 = 0x3c9;
const TOP12_512: u32 = 0x408;
const TOP12_1024: u32 = 0x409;

const LN2_HI: f64 = 0.6931471805598903;
const LN2_LO: f64 = 5.497923018708371e-14;
const INV_LN2_N: f64 = 184.6649652337873;
const SHIFT: f64 = 6755399441055744.0;
const NEG_LN2_HI_N: f64 = -0.005415212348111709;
const NEG_LN2_LO_N: f64 = -1.2864023111638346e-14;

/// Coefficients of log1p, scaled to match the evaluation.
const A: [f64; 7] = [
    -0.5,
    -0.6666666666666679,
    0.5000000000000007,
    0.7999999995323976,
    -0.6666666663487739,
    -1.142909628459501,
    1.0000415263675542,
];

/// Coefficients of exp, from the second-degree one.
const C: [f64; 4] = [
    0.49999999999996786,
    0.16666666666665886,
    0.0416666808410674,
    0.008333335853059549,
];

/// `(1/c, log(c), the rest of log(c))` for each subinterval of [OFF, 2 OFF).
const LOG_TABLE: [(f64, f64, f64); N_LOG] = [
    (1.4140625, -0.3464667673462145, 5.929407345889625e-15),
    (1.40625, -0.34092658697056777, -2.544157440035963e-14),
    (1.3984375, -0.3353555419211034, -3.443525940775045e-14),
    (1.390625, -0.3297532863724655, -2.500123826022799e-15),
    (1.3828125, -0.32411946865420305, -8.929337133850617e-15),
    (1.375, -0.31845373111855224, 1.7625431312172662e-14),
    (1.3671875, -0.31275571000389846, 1.5688303180062087e-15),
    (1.359375, -0.3070250352949415, 2.9655274673691784e-14),
    (1.3515625, -0.3012613305781997, 3.7923164802093147e-14),
    (1.34375, -0.2954642128938758, 3.993416384387844e-14),
    (1.3359375, -0.28963329258306203, 1.9352855826489123e-14),
    (1.3359375, -0.28963329258306203, 1.9352855826489123e-14),
    (1.328125, -0.28376817313062475, -1.9852665484979036e-14),
    (1.3203125, -0.27786845100342816, -2.814323765595281e-14),
    (1.3125, -0.2719337154836694, 2.7643769993528702e-14),
    (1.3046875, -0.2659635484970977, -4.025092402293806e-14),
    (1.296875, -0.25995752443691345, -1.2621729398885316e-14),
    (1.2890625, -0.25391520998095984, -3.600176732637335e-15),
    (1.2890625, -0.25391520998095984, -3.600176732637335e-15),
    (1.28125, -0.2478361639045943, 1.3029797173308663e-14),
    (1.2734375, -0.2417199368871934, 4.8230289429940886e-14),
    (1.265625, -0.23556607131274632, -2.0592242769647135e-14),
    (1.2578125, -0.22937410106487732, 3.149265065191484e-14),
    (1.25, -0.22314355131425145, 4.169796584527195e-14),
    (1.25, -0.22314355131425145, 4.169796584527195e-14),
    (1.2421875, -0.21687393830063684, 2.2477465222466186e-14),
    (1.234375, -0.21056476910735, 3.6507188831790577e-16),
    (1.2265625, -0.2042155414286526, -3.827767260205414e-14),
    (1.2265625, -0.2042155414286526, -3.827767260205414e-14),
    (1.21875, -0.19782574332987224, -4.7641388950792196e-14),
    (1.2109375, -0.19139485299967873, 4.9278276214647115e-14),
    (1.203125, -0.18492233849406148, 4.9485167661250996e-14),
    (1.203125, -0.18492233849406148, 4.9485167661250996e-14),
    (1.1953125, -0.1784076574728033, -1.5003333854266542e-14),
    (1.1875, -0.17185025692663203, -2.7194441649495324e-14),
    (1.1875, -0.17185025692663203, -2.7194441649495324e-14),
    (1.1796875, -0.1652495728952772, -2.99659267292569e-14),
    (1.171875, -0.15860503017665906, 2.0472357800461955e-14),
    (1.171875, -0.15860503017665906, 2.0472357800461955e-14),
    (1.1640625, -0.15191604202584585, 3.879296723063646e-15),
    (1.15625, -0.1451820098444614, -3.6506824353335045e-14),
    (1.1484375, -0.13840232285906495, -5.4183331379008994e-14),
    (1.1484375, -0.13840232285906495, -5.4183331379008994e-14),
    (1.140625, -0.131576357788731, 1.1729485484531301e-14),
    (1.140625, -0.131576357788731, 1.1729485484531301e-14),
    (1.1328125, -0.12470347850091912, -3.811763084710266e-14),
    (1.125, -0.11778303565643, 4.654729747598445e-14),
    (1.125, -0.11778303565643, 4.654729747598445e-14),
    (1.1171875, -0.11081436634026431, -2.5799991283069902e-14),
    (1.109375, -0.10379679368168127, 3.7700471749674615e-14),
    (1.109375, -0.10379679368168127, 3.7700471749674615e-14),
    (1.1015625, -0.09672962645856842, 1.7306161136093256e-14),
    (1.1015625, -0.09672962645856842, 1.7306161136093256e-14),
    (1.09375, -0.089612158689647, -4.012913552726574e-14),
    (1.0859375, -0.08244366921110213, 2.7541708360737882e-14),
    (1.0859375, -0.08244366921110213, 2.7541708360737882e-14),
    (1.078125, -0.07522342123763792, 5.0396178134370583e-14),
    (1.078125, -0.07522342123763792, 5.0396178134370583e-14),
    (1.0703125, -0.06795066190852594, 1.8195060030168815e-14),
    (1.0625, -0.06062462181648698, 5.213620639136504e-14),
    (1.0625, -0.06062462181648698, 5.213620639136504e-14),
    (1.0546875, -0.053244514518837605, 2.532168943117445e-14),
    (1.0546875, -0.053244514518837605, 2.532168943117445e-14),
    (1.046875, -0.045809536031242715, -5.148849572685811e-14),
    (1.046875, -0.045809536031242715, -5.148849572685811e-14),
    (1.0390625, -0.038318864302141264, 4.6652946995830086e-15),
    (1.0390625, -0.038318864302141264, 4.6652946995830086e-15),
    (1.03125, -0.03077165866670839, -4.529814257790929e-14),
    (1.03125, -0.03077165866670839, -4.529814257790929e-14),
    (1.0234375, -0.023167059281490765, -4.361324067851568e-14),
    (1.015625, -0.015504186535963527, -1.7274567499706107e-15),
    (1.015625, -0.015504186535963527, -1.7274567499706107e-15),
    (1.0078125, -0.0077821404420319595, -2.298941004620351e-14),
    (1.0078125, -0.0077821404420319595, -2.298941004620351e-14),
    (1.0, 0.0, 0.0),
    (1.0, 0.0, 0.0),
    (0.9921875, 0.007843177461040796, -1.4902732911301337e-14),
    (0.984375, 0.01574835696817445, -3.527980389655325e-14),
    (0.9765625, 0.023716526617363343, -4.730054772033249e-14),
    (0.96875, 0.03174869831457272, 7.580310369375161e-15),
    (0.9609375, 0.039845908547249564, -4.9893776716773285e-14),
    (0.953125, 0.048009219186383234, -2.262629393030674e-14),
    (0.9453125, 0.056239718322899535, -2.345674491018699e-14),
    (0.94140625, 0.06038051098892083, -1.3352588834854848e-14),
    (0.93359375, 0.06871389254808946, -3.765296820388875e-14),
    (0.92578125, 0.07711730334438016, 5.1128335719851986e-14),
    (0.91796875, 0.08559193033545398, -5.046674438470119e-14),
    (0.9140625, 0.08985632912185793, 3.1218748807418837e-15),
    (0.90625, 0.09844007281321865, 3.3871241029241416e-14),
    (0.8984375, 0.10709813555638448, -1.7376727386423858e-14),
    (0.89453125, 0.11145544092528326, 3.957125899799804e-14),
    (0.88671875, 0.12022742699821265, -5.2849453521890294e-14),
    (0.8828125, 0.12464244520731427, -3.767012502308738e-14),
    (0.875, 0.13353139262449076, 3.1859736349078334e-14),
    (0.87109375, 0.13800567301939282, 5.0900642926060466e-14),
    (0.86328125, 0.14701474296180095, 8.710783796122478e-15),
    (0.859375, 0.15154989812720032, 6.157896229122976e-16),
    (0.8515625, 0.16068238169043525, 3.821577743916796e-14),
    (0.84765625, 0.16528009093906348, 3.9440046718453496e-14),
    (0.83984375, 0.17453941635187675, 2.2924522154618074e-14),
    (0.8359375, 0.17920142945774842, -3.742530094732263e-14),
    (0.83203125, 0.18388527877016259, -2.5223102140407338e-14),
    (0.82421875, 0.1933193110035063, -1.0320443688698849e-14),
    (0.8203125, 0.19806991376208316, 1.0634128304268335e-14),
    (0.8125, 0.20763936477828793, -4.3425422595242564e-14),
    (0.80859375, 0.21245865121420593, -1.2527395755711364e-14),
    (0.8046875, 0.21730127569003344, -5.204008743405884e-14),
    (0.80078125, 0.22216746534115828, -3.979844515951702e-15),
    (0.79296875, 0.2319714654378231, -4.7955860343296286e-14),
    (0.7890625, 0.2369097470783572, 5.015686013791602e-16),
    (0.78515625, 0.24187253642048745, -7.252318953240293e-16),
    (0.78125, 0.2468600779315011, 2.4688324156011588e-14),
    (0.7734375, 0.2569104137850218, 5.465121253624792e-15),
    (0.76953125, 0.26197371574153294, 4.102651071698446e-14),
    (0.765625, 0.2670627852490952, -4.996736502345936e-14),
    (0.76171875, 0.27217788591576664, 4.903580708156347e-14),
    (0.7578125, 0.27731928541618345, 5.089628039500759e-14),
    (0.75390625, 0.28248725557466514, 1.1782016386565151e-14),
    (0.74609375, 0.29290401643288533, 4.727452940514406e-14),
    (0.7421875, 0.29815337231912054, -4.4204083338755686e-14),
    (0.73828125, 0.3034304294199046, 1.548345993498083e-14),
    (0.734375, 0.30873548164959175, 2.1522127491642888e-14),
    (0.73046875, 0.3140688276249648, 1.1054030169005386e-14),
    (0.7265625, 0.31943077076641657, -5.534326352070679e-14),
    (0.72265625, 0.3248216194012912, -5.351646604259541e-14),
    (0.71875, 0.33024168687052224, 5.4612144489920215e-14),
    (0.71484375, 0.3356912916381134, 2.8136969901227338e-14),
    (0.7109375, 0.3411707574027787, -1.156568624616423e-14),
];

/// `2^(k/N)` as a tail and the bits of a scale for each `k`.
const EXP_TABLE: [u64; 2 * N_EXP] = [
    0x0000000000000000,
    0x3ff0000000000000,
    0x3c9b3b4f1a88bf6e,
    0x3feff63da9fb3335,
    0xbc7160139cd8dc5d,
    0x3fefec9a3e778061,
    0xbc905e7a108766d1,
    0x3fefe315e86e7f85,
    0x3c8cd2523567f613,
    0x3fefd9b0d3158574,
    0xbc8bce8023f98efa,
    0x3fefd06b29ddf6de,
    0x3c60f74e61e6c861,
    0x3fefc74518759bc8,
    0x3c90a3e45b33d399,
    0x3fefbe3ecac6f383,
    0x3c979aa65d837b6d,
    0x3fefb5586cf9890f,
    0x3c8eb51a92fdeffc,
    0x3fefac922b7247f7,
    0x3c3ebe3d702f9cd1,
    0x3fefa3ec32d3d1a2,
    0xbc6a033489906e0b,
    0x3fef9b66affed31b,
    0xbc9556522a2fbd0e,
    0x3fef9301d0125b51,
    0xbc5080ef8c4eea55,
    0x3fef8abdc06c31cc,
    0xbc91c923b9d5f416,
    0x3fef829aaea92de0,
    0x3c80d3e3e95c55af,
    0x3fef7a98c8a58e51,
    0xbc801b15eaa59348,
    0x3fef72b83c7d517b,
    0xbc8f1ff055de323d,
    0x3fef6af9388c8dea,
    0x3c8b898c3f1353bf,
    0x3fef635beb6fcb75,
    0xbc96d99c7611eb26,
    0x3fef5be084045cd4,
    0x3c9aecf73e3a2f60,
    0x3fef54873168b9aa,
    0xbc8fe782cb86389d,
    0x3fef4d5022fcd91d,
    0x3c8a6f4144a6c38d,
    0x3fef463b88628cd6,
    0x3c807a05b0e4047d,
    0x3fef3f49917ddc96,
    0x3c968efde3a8a894,
    0x3fef387a6e756238,
    0x3c875e18f274487d,
    0x3fef31ce4fb2a63f,
    0x3c80472b981fe7f2,
    0x3fef2b4565e27cdd,
    0xbc96b87b3f71085e,
    0x3fef24dfe1f56381,
    0x3c82f7e16d09ab31,
    0x3fef1e9df51fdee1,
    0xbc3d219b1a6fbffa,
    0x3fef187fd0dad990,
    0x3c8b3782720c0ab4,
    0x3fef1285a6e4030b,
    0x3c6e149289cecb8f,
    0x3fef0cafa93e2f56,
    0x3c834d754db0abb6,
    0x3fef06fe0a31b715,
    0x3c864201e2ac744c,
    0x3fef0170fc4cd831,
    0x3c8fdd395dd3f84a,
    0x3feefc08b26416ff,
    0xbc86a3803b8e5b04,
    0x3feef6c55f929ff1,
    0xbc924aedcc4b5068,
    0x3feef1a7373aa9cb,
    0xbc9907f81b512d8e,
    0x3feeecae6d05d866,
    0xbc71d1e83e9436d2,
    0x3feee7db34e59ff7,
    0xbc991919b3ce1b15,
    0x3feee32dc313a8e5,
    0x3c859f48a72a4c6d,
    0x3feedea64c123422,
    0xbc9312607a28698a,
    0x3feeda4504ac801c,
    0xbc58a78f4817895b,
    0x3feed60a21f72e2a,
    0xbc7c2c9b67499a1b,
    0x3feed1f5d950a897,
    0x3c4363ed60c2ac11,
    0x3feece086061892d,
    0x3c9666093b0664ef,
    0x3feeca41ed1d0057,
    0x3c6ecce1daa10379,
    0x3feec6a2b5c13cd0,
    0x3c93ff8e3f0f1230,
    0x3feec32af0d7d3de,
    0x3c7690cebb7aafb0,
    0x3feebfdad5362a27,
    0x3c931dbdeb54e077,
    0x3feebcb299fddd0d,
    0xbc8f94340071a38e,
    0x3feeb9b2769d2ca7,
    0xbc87deccdc93a349,
    0x3feeb6daa2cf6642,
    0xbc78dec6bd0f385f,
    0x3feeb42b569d4f82,
    0xbc861246ec7b5cf6,
    0x3feeb1a4ca5d920f,
    0x3c93350518fdd78e,
    0x3feeaf4736b527da,
    0x3c7b98b72f8a9b05,
    0x3feead12d497c7fd,
    0x3c9063e1e21c5409,
    0x3feeab07dd485429,
    0x3c34c7855019c6ea,
    0x3feea9268a5946b7,
    0x3c9432e62b64c035,
    0x3feea76f15ad2148,
    0xbc8ce44a6199769f,
    0x3feea5e1b976dc09,
    0xbc8c33c53bef4da8,
    0x3feea47eb03a5585,
    0xbc845378892be9ae,
    0x3feea34634ccc320,
    0xbc93cedd78565858,
    0x3feea23882552225,
    0x3c5710aa807e1964,
    0x3feea155d44ca973,
    0xbc93b3efbf5e2228,
    0x3feea09e667f3bcd,
    0xbc6a12ad8734b982,
    0x3feea012750bdabf,
    0xbc6367efb86da9ee,
    0x3fee9fb23c651a2f,
    0xbc80dc3d54e08851,
    0x3fee9f7df9519484,
    0xbc781f647e5a3ecf,
    0x3fee9f75e8ec5f74,
    0xbc86ee4ac08b7db0,
    0x3fee9f9a48a58174,
    0xbc8619321e55e68a,
    0x3fee9feb564267c9,
    0x3c909ccb5e09d4d3,
    0x3feea0694fde5d3f,
    0xbc7b32dcb94da51d,
    0x3feea11473eb0187,
    0x3c94ecfd5467c06b,
    0x3feea1ed0130c132,
    0x3c65ebe1abd66c55,
    0x3feea2f336cf4e62,
    0xbc88a1c52fb3cf42,
    0x3feea427543e1a12,
    0xbc9369b6f13b3734,
    0x3feea589994cce13,
    0xbc805e843a19ff1e,
    0x3feea71a4623c7ad,
    0xbc94d450d872576e,
    0x3feea8d99b4492ed,
    0x3c90ad675b0e8a00,
    0x3feeaac7d98a6699,
    0x3c8db72fc1f0eab4,
    0x3feeace5422aa0db,
    0xbc65b6609cc5e7ff,
    0x3feeaf3216b5448c,
    0x3c7bf68359f35f44,
    0x3feeb1ae99157736,
    0xbc93091fa71e3d83,
    0x3feeb45b0b91ffc6,
    0xbc5da9b88b6c1e29,
    0x3feeb737b0cdc5e5,
    0xbc6c23f97c90b959,
    0x3feeba44cbc8520f,
    0xbc92434322f4f9aa,
    0x3feebd829fde4e50,
    0xbc85ca6cd7668e4b,
    0x3feec0f170ca07ba,
    0x3c71affc2b91ce27,
    0x3feec49182a3f090,
    0x3c6dd235e10a73bb,
    0x3feec86319e32323,
    0xbc87c50422622263,
    0x3feecc667b5de565,
    0x3c8b1c86e3e231d5,
    0x3feed09bec4a2d33,
    0xbc91bbd1d3bcbb15,
    0x3feed503b23e255d,
    0x3c90cc319cee31d2,
    0x3feed99e1330b358,
    0x3c8469846e735ab3,
    0x3feede6b5579fdbf,
    0xbc82dfcd978e9db4,
    0x3feee36bbfd3f37a,
    0x3c8c1a7792cb3387,
    0x3feee89f995ad3ad,
    0xbc907b8f4ad1d9fa,
    0x3feeee07298db666,
    0xbc55c3d956dcaeba,
    0x3feef3a2b84f15fb,
    0xbc90a40e3da6f640,
    0x3feef9728de5593a,
    0xbc68d6f438ad9334,
    0x3feeff76f2fb5e47,
    0xbc91eee26b588a35,
    0x3fef05b030a1064a,
    0x3c74ffd70a5fddcd,
    0x3fef0c1e904bc1d2,
    0xbc91bdfbfa9298ac,
    0x3fef12c25bd71e09,
    0x3c736eae30af0cb3,
    0x3fef199bdd85529c,
    0x3c8ee3325c9ffd94,
    0x3fef20ab5fffd07a,
    0x3c84e08fd10959ac,
    0x3fef27f12e57d14b,
    0x3c63cdaf384e1a67,
    0x3fef2f6d9406e7b5,
    0x3c676b2c6c921968,
    0x3fef3720dcef9069,
    0xbc808a1883ccb5d2,
    0x3fef3f0b555dc3fa,
    0xbc8fad5d3ffffa6f,
    0x3fef472d4a07897c,
    0xbc900dae3875a949,
    0x3fef4f87080d89f2,
    0x3c74a385a63d07a7,
    0x3fef5818dcfba487,
    0xbc82919e2040220f,
    0x3fef60e316c98398,
    0x3c8e5a50d5c192ac,
    0x3fef69e603db3285,
    0x3c843a59ac016b4b,
    0x3fef7321f301b460,
    0xbc82d52107b43e1f,
    0x3fef7c97337b9b5f,
    0xbc892ab93b470dc9,
    0x3fef864614f5a129,
    0x3c74b604603a88d3,
    0x3fef902ee78b3ff6,
    0x3c83c5ec519d7271,
    0x3fef9a51fbc74c83,
    0xbc8ff7128fd391f0,
    0x3fefa4afa2a490da,
    0xbc8dae98e223747d,
    0x3fefaf482d8e67f1,
    0x3c8ec3bc41aa2008,
    0x3fefba1bee615a27,
    0x3c842b94c3a9eb32,
    0x3fefc52b376bba97,
    0x3c8a64a931d185ee,
    0x3fefd0765b6e4540,
    0xbc8e37bae43be3ed,
    0x3fefdbfdad9cbe14,
    0x3c77893b4d91cd9d,
    0x3fefe7c1819e90d8,
    0x3c5305c14160cc89,
    0x3feff3c22b8f71f1,
];

fn top12(x: f64) -> u32 {
    (x.to_bits() >> 52) as u32
}

/// Returns `log(x)` as `(y, tail)`, where `y` is the rounded result and `tail`
/// has about 15 bits more of precision. `ix` is the bit representation of x,
/// normalized in the subnormal range using the sign bit for the exponent.
fn log_inline(ix: u64) -> (f64, f64) {
    // x = 2^k z, where z is in [OFF, 2 OFF) and exact. The range is split
    // into N subintervals; the ith one contains z and c is near its center.
    let tmp = ix.wrapping_sub(OFF);
    let i = ((tmp >> (52 - 7)) % N_LOG as u64) as usize;
    let k = (tmp as i64) >> 52;
    let iz = ix.wrapping_sub(tmp & (0xfff << 52));
    let z = f64::from_bits(iz);
    let kd = k as f64;

    // log(x) = k*Ln2 + log(c) + log1p(z/c-1).
    let (invc, logc, logctail) = LOG_TABLE[i];

    // Split z such that rhi, rlo and rhi*rhi are exact and |rlo| <= |r|.
    let zhi = f64::from_bits(iz.wrapping_add(1 << 31) & (u64::MAX << 32));
    let zlo = z - zhi;
    let rhi = zhi * invc - 1.0;
    let rlo = zlo * invc;
    let r = rhi + rlo;

    // k*Ln2 + log(c) + r.
    let t1 = kd * LN2_HI + logc;
    let t2 = t1 + r;
    let lo1 = kd * LN2_LO + logctail;
    let lo2 = t1 - t2 + r;

    // k*Ln2 + log(c) + r + A[0]*r*r.
    let ar = A[0] * r;
    let ar2 = r * ar;
    let ar3 = r * ar2;
    let arhi = A[0] * rhi;
    let arhi2 = rhi * arhi;
    let hi = t2 + arhi2;
    let lo3 = rlo * (ar + arhi);
    let lo4 = t2 - hi + arhi2;

    // p = log1p(r) - r - A[0]*r*r.
    let p = ar3 * (A[1] + r * A[2] + ar2 * (A[3] + r * A[4] + ar2 * (A[5] + r * A[6])));
    let lo = lo1 + lo2 + lo3 + lo4 + p;
    let y = hi + lo;
    (y, hi - y + lo)
}

/// Handles the cases that may overflow or underflow when computing the result
/// `scale*(1+tmp)` without intermediate rounding. `sbits` is the bit
/// representation of scale, with a computed exponent that may have overflown
/// into the sign bit. `ki` is the k of the argument reduction: positive means
/// the result may overflow, and negative that it may underflow.
fn special_case(tmp: f64, sbits: u64, ki: u64) -> f64 {
    if ki & 0x80000000 == 0 {
        // k > 0, the exponent of scale might have overflowed by <= 460.
        let scale = f64::from_bits(sbits.wrapping_sub(1009 << 52));
        return f64::from_bits((1023 + 1009) << 52) * (scale + scale * tmp);
    }
    // k < 0, need special care in the subnormal range.
    let sbits = sbits.wrapping_add(1022 << 52);
    let scale = f64::from_bits(sbits);
    let mut y = scale + scale * tmp;
    if libm::fabs(y) < 1.0 {
        // Round y to the right precision before scaling it into the subnormal
        // range to avoid double rounding.
        let one = if y < 0.0 { -1.0 } else { 1.0 };
        let lo = scale - y + scale * tmp;
        let hi = one + y;
        let lo = one - hi + y + lo;
        y = (hi + lo) - one;
        // Fix the sign of 0.
        if y == 0.0 {
            y = f64::from_bits(sbits & 0x8000000000000000);
        }
    }
    f64::MIN_POSITIVE * y
}

/// Computes `sign*exp(x+xtail)`, where |xtail| < 2^-8/N and |xtail| <= |x|.
/// `sign_bias` is `SIGN_BIAS` or 0 and sets the sign to -1 or 1.
fn exp_inline(x: f64, xtail: f64, sign_bias: u64) -> f64 {
    let mut abstop = top12(x) & 0x7ff;
    if abstop.wrapping_sub(TOP12_TINY) >= TOP12_512 - TOP12_TINY {
        if abstop.wrapping_sub(TOP12_TINY) >= 0x80000000 {
            // Avoid spurious underflow for tiny x.
            let one = 1.0 + x;
            return if sign_bias != 0 { -one } else { one };
        }
        if abstop >= TOP12_1024 {
            // inf and nan are already handled.
            let huge = if x.is_sign_negative() {
                0.0
            } else {
                f64::INFINITY
            };
            return if sign_bias != 0 { -huge } else { huge };
        }
        // Large x is special cased below.
        abstop = 0;
    }

    // exp(x) = 2^(k/N) * exp(r), with exp(r) in [2^(-1/2N),2^(1/2N)].
    // x = ln2/N*k + r, with int k and r in [-ln2/2N, ln2/2N].
    let z = INV_LN2_N * x;
    let kd = z + SHIFT;
    let ki = kd.to_bits();
    let kd = kd - SHIFT;
    let r = x + kd * NEG_LN2_HI_N + kd * NEG_LN2_LO_N + xtail;
    // 2^(k/N) ~= scale * (1 + tail).
    let idx = 2 * (ki % N_EXP as u64) as usize;
    let top = ki.wrapping_add(sign_bias) << (52 - 7);
    let tail = f64::from_bits(EXP_TABLE[idx]);
    // This is only a valid scale when -1023*N < k < 1024*N.
    let sbits = EXP_TABLE[idx + 1].wrapping_add(top);
    // exp(x) = 2^(k/N) * exp(r) ~= scale + scale * (tail + exp(r) - 1).
    let r2 = r * r;
    let tmp = tail + r + r2 * (C[0] + r * C[1]) + r2 * r2 * (C[2] + r * C[3]);
    if abstop == 0 {
        return special_case(tmp, sbits, ki);
    }
    let scale = f64::from_bits(sbits);
    scale + scale * tmp
}

/// Returns 0 if not int, 1 if odd int, 2 if even int. The argument is the bit
/// representation of a non-zero finite floating-point value.
fn checkint(iy: u64) -> u32 {
    let e = (iy >> 52 & 0x7ff) as u32;
    if e < 0x3ff {
        return 0;
    }
    if e > 0x3ff + 52 {
        return 2;
    }
    if iy & ((1 << (0x3ff + 52 - e)) - 1) != 0 {
        return 0;
    }
    if iy & (1 << (0x3ff + 52 - e)) != 0 {
        return 1;
    }
    2
}

/// Returns whether the input is the bit representation of 0, infinity or nan.
fn zeroinfnan(i: u64) -> bool {
    (i << 1).wrapping_sub(1) >= 2 * f64::INFINITY.to_bits() - 1
}

pub fn pow(x: f64, y: f64) -> f64 {
    let mut sign_bias = 0;
    let mut ix = x.to_bits();
    let iy = y.to_bits();
    let mut topx = top12(x);
    let topy = top12(y);
    if topx.wrapping_sub(0x001) >= 0x7ff - 0x001
        || (topy & 0x7ff).wrapping_sub(0x3be) >= 0x43e - 0x3be
    {
        // If |y| > 1075 * ln2 * 2^53 ~= 0x1.749p62 then pow(x,y) = inf/0 and
        // if |y| < 2^-54 / 1075 ~= 0x1.e7b6p-65 then pow(x,y) = +-1.
        // Special cases: (x < 0x1p-126 or inf or nan) or
        // (|y| < 0x1p-65 or |y| >= 0x1p63 or nan).
        if zeroinfnan(iy) {
            if (iy << 1) == 0 || ix == 1.0f64.to_bits() {
                return 1.0;
            }
            if (ix << 1) > 2 * f64::INFINITY.to_bits() || (iy << 1) > 2 * f64::INFINITY.to_bits() {
                return x + y;
            }
            if (ix << 1) == 2 * 1.0f64.to_bits() {
                return 1.0;
            }
            if ((ix << 1) < 2 * 1.0f64.to_bits()) == (iy >> 63 == 0) {
                // |x|<1 && y==inf or |x|>1 && y==-inf.
                return 0.0;
            }
            return y * y;
        }
        if zeroinfnan(ix) {
            let mut x2 = x * x;
            if ix >> 63 != 0 && checkint(iy) == 1 {
                x2 = -x2;
            }
            return if iy >> 63 != 0 { 1.0 / x2 } else { x2 };
        }
        // Here x and y are non-zero finite.
        if ix >> 63 != 0 {
            // Finite x < 0.
            match checkint(iy) {
                0 => return f64::NAN,
                1 => sign_bias = SIGN_BIAS,
                _ => {}
            }
            ix &= 0x7fffffffffffffff;
            topx &= 0x7ff;
        }
        if (topy & 0x7ff).wrapping_sub(0x3be) >= 0x43e - 0x3be {
            // sign_bias == 0 here because y is not odd.
            if ix == 1.0f64.to_bits() {
                return 1.0;
            }
            if (topy & 0x7ff) < 0x3be {
                // |y| < 2^-65, x^y ~= 1 + y*log(x).
                return if ix > 1.0f64.to_bits() {
                    1.0 + y
                } else {
                    1.0 - y
                };
            }
            return if (ix > 1.0f64.to_bits()) == (topy < 0x800) {
                f64::INFINITY
            } else {
                0.0
            };
        }
        if topx == 0 {
            // Normalize subnormal x so exponent becomes negative.
            ix = (x * 4503599627370496.0).to_bits();
            ix &= 0x7fffffffffffffff;
            ix = ix.wrapping_sub(52 << 52);
        }
    }

    let (hi, lo) = log_inline(ix);
    let yhi = f64::from_bits(iy & (u64::MAX << 27));
    let ylo = y - yhi;
    let lhi = f64::from_bits(hi.to_bits() & (u64::MAX << 27));
    let llo = hi - lhi + lo;
    let ehi = yhi * lhi;
    // |elo| < |ehi| * 2^-25.
    let elo = ylo * lhi + y * llo;
    exp_inline(ehi, elo, sign_bias)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::num::Xoshiro256;

    fn same(a: f64, b: f64) -> bool {
        a.to_bits() == b.to_bits() || (a.is_nan() && b.is_nan())
    }

    #[test]
    fn special_cases() {
        let cases: &[(f64, f64, f64)] = &[
            (0.0, 0.0, 1.0),
            (f64::NAN, 0.0, 1.0),
            (1.0, f64::NAN, 1.0),
            (2.0, f64::NAN, f64::NAN),
            (f64::NAN, 2.0, f64::NAN),
            (-1.0, f64::INFINITY, 1.0),
            (0.5, f64::INFINITY, 0.0),
            (2.0, f64::INFINITY, f64::INFINITY),
            (2.0, f64::NEG_INFINITY, 0.0),
            (0.5, f64::NEG_INFINITY, f64::INFINITY),
            (0.0, 3.0, 0.0),
            (-0.0, 3.0, -0.0),
            (-0.0, 2.0, 0.0),
            (0.0, -1.0, f64::INFINITY),
            (-0.0, -1.0, f64::NEG_INFINITY),
            (f64::INFINITY, -2.0, 0.0),
            (f64::NEG_INFINITY, 3.0, f64::NEG_INFINITY),
            (-2.0, 0.5, f64::NAN),
            (-2.0, 3.0, -8.0),
            (-2.0, 2.0, 4.0),
            (2.0, 1e300, f64::INFINITY),
            (0.5, 1e300, 0.0),
            (2.0, 1e-300, 1.0),
            (10.0, 308.0, 1e308),
            (10.0, 309.0, f64::INFINITY),
            (2.0, -1074.0, 5e-324),
            (2.0, -1075.0, 0.0),
            (5e-324, 0.5, 2.2227587494850775e-162),
        ];
        for &(x, y, expected) in cases {
            assert!(
                same(pow(x, y), expected),
                "pow({x:e}, {y:e}) = {:e}",
                pow(x, y)
            );
        }
    }

    /// The C library of the test machine, glibc ≥ 2.28 or musl, has the same
    /// algorithm, but it may be built with fused multiply-add, which changes
    /// the rounding of about 0.02% of the results (glibc 2.41 on x86-64).
    #[test]
    fn same_as_the_c_library() {
        let mut rng = Xoshiro256::new([1, 2, 3, 4]);
        let mut different = 0;
        let n = 1_000_000;
        for k in 0..n {
            let x = match k % 3 {
                0 => rng.next_f64() * 100.0,
                1 => 10f64.powf(rng.next_f64() * 600.0 - 300.0),
                _ => f64::from_bits(rng.next_u64() & 0x7fefffffffffffff),
            };
            let y = match k % 4 {
                0 => rng.next_f64() * 10.0 - 5.0,
                1 => (rng.next_f64() * 20.0 - 10.0).round(),
                2 => rng.next_f64() * 2000.0 - 1000.0,
                _ => f64::from_bits(rng.next_u64()),
            };
            if !same(pow(x, y), x.powf(y)) {
                different += 1;
            }
        }
        assert!(different * 2_000 < n, "{different} of {n}");
    }
}
