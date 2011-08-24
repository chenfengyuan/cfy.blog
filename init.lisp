(ql:quickload "swank")
(swank:create-server :port 4006 :style :spawn :dont-close t :coding-system "utf-8-unix")