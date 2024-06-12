#-asdf3.1 (error "need 3.1 or above")

(defsystem "mbfamilysys"
  :description "Michael Bi's family system"
  :author "Michael Bi"
  :license "Unlicensed"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :depends-on ("reblocks"
	       "reblocks-navigation-widget"
	       "reblocks-ui"
	       "mito")
  :pathname "src")
