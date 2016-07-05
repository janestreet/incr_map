#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"incr_map"
  [ oasis_lib "incr_map"
  ; file "META" ~section:"lib"
  ]
