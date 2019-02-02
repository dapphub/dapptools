let
   mk1 = import ./post-0.4.5.nix;
   mk2 = import ./post-0.4.17.nix;
   mk3 = import ./post-0.4.24.nix;
   mk4 = import ./post-0.5.1.nix;
   mk5 = import ./post-0.5.2.nix;
in {

  ## I looked up the hashes for all the older versions, but prior to
  ## 0.3.6 the Solc repository was not standalone and I haven't even
  ## tried getting them to build.

  ## In fact 0.4.5 is the first version that compiles with our build
  ## script.  Prior versions could probably be made to work, but for
  ## now I disable them.

  ## Also note that in v0.4.9 the compiler started prefixing things
  ## with "filename:", so that's the oldest compiler some things will
  ## work with.

  # solc_0_1_2  = mk "0.1.2" "0906042ce05f01c4d371aa98d0fd9dddfb93a196" "12498k4skfv3a6jlvm97p1v9c77nfj5910305a19pl9s7w38gbcy";
  # solc_0_1_3  = mk "0.1.3" "028f561dac6aec0692daa8217b0ee1e3a0c5aa72" "1ax3hj5varljk4rl6by63v3ymigm2r8sjx4wl39ia118xcvcl7mp";
  # solc_0_1_4  = mk "0.1.4" "63a9c4f8960664af056f9b41c24e6b5d74edcb14" "1pga8xg8klxnilhy9kx0vf4989xhykc4p0avfzgl8n7r2h6vqmw5";
  # solc_0_1_5  = mk "0.1.5" "23865e39295dd9199769727b037c1d126807b20e" "16dr9isr8dfhb7rps76kb406ac1z5ibaxr0w0w1mjw8j47qbx8sw";
  # solc_0_1_6  = mk "0.1.6" "d41f8b7ce702c3b25c48d27e2e895ccdcd04e4e0" "173s5f04ii7px85qil7wqypx8z8vx9x6f031ak42ysjf82dqv4y9";
  # solc_0_1_7  = mk "0.1.7" "b4e666ccf4fe39b0a1fc909b61daf6dc39e77fc4" "0h2i6x9yjqjmhzjcni0nw8jqdqqfvnbxdnc6dn3hl8nx0778ri80";
  # solc_0_2_0  = mk "0.2.0" "4dc2445edd05d9d6bdc9fc4673ad91ead11f1632" "1npcfn68ykhwmq907g3qq72z4zd4j1nm81ahvw32c6m4pivq0idl";
  # solc_0_2_1  = mk "0.2.1" "91a6b35f18c9e3ae38b08460ba3b0d3590e573dc" "0d0m3nc6sh1wwngz1sxfan9g7nf9jsdqfxclvpw3vsiygj1y9v3c";
  # solc_0_2_2  = mk "0.2.2" "ef92f5661b06f7a277aec4b76f3eb62772239e69" "0s5mkkvyr19f5zw4xhpxccgf9a5q9fbp0qnd2zzkfvzsl6c9xy1s";
  # solc_0_3_0  = mk "0.3.0" "1f9578cea3f7ea1982ba2288cd3238bfe791b348" "0ikix08y6l4izcvkjrydqwprxwiz1102gl4s80dm63r8crd15c3v";
  # solc_0_3_1  = mk "0.3.1" "c67926cf2be95e9275eb2b7fe904131e7e8842ad" "1v3s9h57c6w8lcdcy49g2p5msip026sm4zsv8pncwfqw0fchdric";
  # solc_0_3_2  = mk "0.3.2" "81ae2a78321fddcd2d32efc51568ebeca28866a8" "0k5b15ywx324lajg2hmp8p527qp6ai1ak4wr7s4rdw4s0ljyjqz8";
  # solc_0_3_3  = mk "0.3.3" "4dc1cb149c7e3677119b052ae0f7dc7a019fa952" "0flp7i58hx56rf431n9lxq39r65n7f4p7y9v7bpii2sqrz1af9a7";
  # solc_0_3_4  = mk "0.3.4" "7dab8902789b15190e39d4fed0bc46418f1ace5e" "11w51phzaac96x9gz7r2ac6wkpry9yhav6a0rahcr76rvyxmhj2b";
  # solc_0_3_5  = mk "0.3.5" "5f97274a6cc2d9346357d69b2c4a9348cdffa7f9" "147sq3zh1cqhkinxcwv5i1la444y8bgn92fcac7xd84qddn3yd6n";

  # solc_0_3_6  = mk "0.3.6" "988fe5e5aa271d89e0700d36ef0f210ca50051e3" "1lg7knx9a9488yhl2gavaq8g9ywvskp73sd31fc8ql15qf4ay83r";
  # solc_0_4_0  = mk "0.4.0" "acd334c9d289a7cc4674dde2534d8728450cedde" "0i63bcclcs8i2j90p8l3hvmvjmad9rh8vkfz1l4w1c2501ani971";
  # solc_0_4_1  = mk "0.4.1" "4fc6fc2ca59579fae2472df319c2d8d31fe5bde5" "0ixrkp2y49lf4av52ik6ywngmg53vigj8pk10jwd94ld1aw2bcpd";
  # solc_0_4_2  = mk "0.4.2" "af6afb0415761b53721f89c7f65064807f41cbd3" "1086w2hkjlqh73p62nnrkcwxsiiy2q2mz3r4zgppbrn717cq68v5";
  # solc_0_4_3  = mk "0.4.3" "2353da71c77dd235b35d16e7e024fa62408df610" "06m0zhidnd8vc56247yw7zqg6bk6girgr0cv9fh7avkfcwjpn42g";
  # solc_0_4_4  = mk "0.4.4" "4633f3def897db0f91237f98cf46e5d84fb05e61" "1z32a9vb6rjxmg5cqpkm45xbjq6nyx9p31m7cjppqw6zljw6zjzs";

  solc_0_4_5  = mk1 "0.4.5"   "b318366e6f16ed6a4274247d09badac4affff8d5" "1lnmbqv4qqdc6077i27471mg9sr34d4wd9505w8wj94scjq3mpxm";
  solc_0_4_6  = mk1 "0.4.6"   "2dabbdf06f414750ef0425c664f861aeb3e470b8" "0q1dvizx60f7l97w8241wra7vpghimc9x7gzb18vn34sxv4bqy9g";
  solc_0_4_7  = mk1 "0.4.7"   "822622cf5bf23e79a6e2292cb837d1a39ca1c419" "1x2s9gi9y8h03j7nv6wyjjbw74vnzzhr206nik0qasyajgrb0sk2";
  solc_0_4_8  = mk1 "0.4.8"   "60cc1668517f56ce6ca8225555472e7a27eab8b0" "09mwah7c5ca1bgnqp5qgghsi6mbsi7p16z8yxm0aylsn2cjk23na";
  solc_0_4_9  = mk1 "0.4.9"   "364da425d3116a4b85863df39a1864340861d71e" "1qlc3isbdvg4llvqxcdfhqx1m4z80jd7iw89sivz2nga4nx06wwz";
  solc_0_4_10 = mk1 "0.4.10"  "f0d539ae05739e35336cc9cc8f44bd9798a95c28" "04kmibc4q0sr5qyawbp855iix79di6ynix7vpsfx9gjwdddjk6j8";
  solc_0_4_11 = mk1 "0.4.11"  "68ef5810593e7c8092ed41d5f474dd43141624eb" "13zycybf23yvf3hkf9zgw9gbc1y4ifzxaf7sll69bsn24fcyq961";
  solc_0_4_12 = mk1 "0.4.12"  "194ff033ae44944ac59aa7bd3da89ba94ec5893c" "0gkg3nay0625qmhxxxax1d1c4dl554ri3pkwd12qfg6g1w6j04w7";
  solc_0_4_13 = mk1 "0.4.13"  "0fb4cb1ab9bb4b6cc72e28cc5a1753ad14781f14" "0rhrm0bmk5s2358j40yx7dzr1938q17dchzflrxw6y7yvkhscxrm";
  solc_0_4_14 = mk1 "0.4.14"  "c2215d4605d1fbcef1366d6b822ec610fc031b3c" "0pfn0b8nmdp61ig2g1jnhy4sdlxvkrhv4pw237zyvs97sjnll377";
  solc_0_4_15 = mk1 "0.4.15"  "8b45bddb559d17250c8a5619efa1a21f296d4e03" "0a1gy4j3yximb7ja7q0ldwg34h6759dmdkfjb87nqfizj05cg5q3";
  solc_0_4_16 = mk1 "0.4.16"  "d7661dd97460250b4e1127b9e7ea91e116143780" "1fd69pdhkkkvbkrxipkck1icpqkpdskjzar48a1yzdsx3l8s4lil";
  solc_0_4_17 = mk2 "0.4.17"  "bdeb9e52a2211510644fb53df93fb98258b40a65" "1x6q2rlq6gxggidgsy6li7m4phwr1hcfi65pq9yimz64ddqfiira";
  solc_0_4_18 = mk2 "0.4.18"  "9cf6e910bd2b90d0c9415d9c257f85fe0c518de8" "0ij7qbn3ci6v4jf4gqcdphwy8lnc1l4ycw9pvq6c80kd1fayf2s6";
  solc_0_4_19 = mk2 "0.4.19"  "c4cbbb054b5ed3b8ceaa21ee5b47b0704762ff40" "1h2ziwdswghj4aa3vd3k3y2ckfiwjk6x38w2kp4m324k2ydxd15c";
  solc_0_4_20 = mk2 "0.4.20"  "3155dd8058672ce8f04bc2c0f2536cb549067d0a" "0jgqi6rnyr8d3plbag1p0yp1s1fzvjjsk4yrv06v46bsvyx4lgcn";
  solc_0_4_21 = mk2 "0.4.21"  "dfe3193c7382c80f1814247a162663a97c3f5e67" "0gbf3r6waqsp76aaql779jw9817sgvw4vdlrrpq0l1r1nm82lxq5";
  solc_0_4_22 = mk2 "0.4.22"  "4cb486ee993cadde5564fb6c611d2bcf4fc44414" "05zrqf1pq4khjhvlk3nbg86s8f8dnzikd20ln1sy83pl9fchc0h3";
  solc_0_4_23 = mk2 "0.4.23"  "124ca40dc525a987a88176c6e5170978e82fa290" "07l8rfqh95yrdmbxc4pfb77s06k5v65dk3rgdqscqmwchkndrmm0";
  solc_0_4_24 = mk3 "0.4.24"  "e67f0147998a9e3835ed3ce8bf6a0a0c634216c5" "1gy2miv6ia1z98zy6w4y03balwfr964bnvwzyg8v7pn2mayqnaap";
  solc_0_4_25 = mk3 "0.4.25"  "59dbf8f1085b8b92e8b7eb0ce380cbeb642e97eb" "11lss1sldzjg4689c06iw0iivyi9f4zpi4l9za0fgy6k85qz43v9";
  solc_0_5_0  = mk3 "0.5.0"   "1d4f565a64988a3400847d2655ca24f73f234bc6" "0phzk2whvgrrf8xpl5pz886glhd5s40y1hbbvq9q3fxf6vc3lisy";
  solc_0_5_1  = mk4 "0.5.1"   "c8a2cb62832afb2dc09ccee6fd42c1516dfdb981" "0d6mfnixlr9m5yr3r4p6cv6vwrrivcamyar5d0f9rvir9w9ypzrr";
  solc_0_5_2  = mk5 "0.5.2"   "1df8f40cd2fd7b47698d847907b8ca7b47eb488d" "009kjyb3r2p64wpdzfcmqr9swm5haaixbzvsbw1nd4wipwbp66y0";
}
