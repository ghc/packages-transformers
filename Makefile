TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS =

ALL_DIRS =                  \
	Control/Monad           \
	Control/Monad/Trans     \
	Control/Monad/Trans/RWS \
	Control/Monad/Trans/State \
	Control/Monad/Trans/Writer

PACKAGE = mtl
VERSION = 1.99
PACKAGE_DEPS = base

EXCLUDED_SRCS += Setup.hs

SRC_HC_OPTS += -Wall -fglasgow-exts

SRC_HADDOCK_OPTS += -t "Haskell Hierarchical Libraries ($(PACKAGE) package)"

include $(TOP)/mk/target.mk
