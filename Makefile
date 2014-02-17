Main: Main.hs Application.hs Routes.hs MustacheTemplates.hs PathHelpers.hs Stripe.hs
	ghc -threaded -O2 -Wall -fno-warn-name-shadowing Main.hs

Routes.hs: routes
	routeGenerator -r -m Application -n 4 $< > $@

PathHelpers.hs: routes
	routeGenerator -p -n 4 $< > $@

MustacheTemplates.hs: Records.hs view/home.mustache view/depositSuccess.mustache view/plivoDeposit.mustache view/depositVerify.mustache view/quoteSuccess.mustache view/meta.mustache view/header.mustache
	mustache2hs -m Records.hs view/home.mustache Home view/depositSuccess.mustache DepositSuccess view/plivoDeposit.mustache PlivoDeposit view/depositVerify.mustache Home view/quoteSuccess.mustache QuoteSuccess view/meta.mustache Header view/header.mustache Header > $@

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist dist-ghc Main Routes.hs PathHelpers.hs MustacheTemplates.hs
