module Stripe (verifyCard, StripeConfig, RequestCard(..)) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (throwError)
import Web.Stripe.Customer
import Web.Stripe.Client
import Web.Stripe.Card
import qualified Data.Text as T

s :: String -> T.Text
s = T.pack

-- | Creates a new Stripe customer and returns their id,
--   verifying that all information is valid and the card is Canadian.
verifyCard :: (MonadIO m) =>
	StripeConfig    -- ^ Stripe secret key 
	-> Maybe String -- ^ Customer description (for debugging)
	-> Maybe String -- ^ Customer email (for debugging)
	-> RequestCard  -- ^ Credit card
	-> m (Either StripeFailure T.Text)
verifyCard config descr email card = runStripeT config $ do
	cust <- createCustomer (Just card) Nothing
		(fmap (Email . T.pack) email)
		(fmap (Description . T.pack) descr)
		Nothing Nothing
	case custActiveCard cust of
		Just (Card {
			cardCountry = Just country,
			cardChecks = CardChecks Passed Passed Passed
		}) | country == s"CA" -> return $ unCustomerId $ custId cust
		   | otherwise -> throwError $ PaymentRequired $ Just $
			CardError (s"The card you supplied is not from Canada.")
			(UnknownErrorCode $ s"incorrect_country") (Just $ s"address_country")
		Just (Card {
			cardChecks = CardChecks Passed _ Passed
		}) -> throwError $ PaymentRequired $ Just $
			CardError (s"The address you supplied failed validation.")
			(UnknownErrorCode $ s"incorrect_line1") (Just $ s"address_line1")
		_ -> throwError $ PaymentRequired $ Just $
			CardError (s"Unknown validation failure.")
			(UnknownErrorCode $ s"unknown") Nothing
