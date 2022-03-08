# Testing the relatable numbers function


identical(relatable_number(365*24*60*60), "one person every second")

identical(relatable_number(365*24*60), "one person every 60 seconds")

identical(relatable_number(365*24*60 - 1), "one person every minute")

identical(relatable_number(365*24), "one person every 60 minutes")

identical(relatable_number(365*24- 1), "one person every hour")

identical(relatable_number(450), "one person every 19 hours")

identical(relatable_number(365), "one person every 24 hours")

identical(relatable_number(364), "one person every day")

identical(relatable_number(100), "one person every 4 days")


