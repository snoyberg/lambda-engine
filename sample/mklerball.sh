ghc --make sample.hs && strip sample && chmod -x sample && tar cjfv sample.tar.bz2 sample lambda-engine.xml static/ && mv sample.tar.bz2 ../incoming/
