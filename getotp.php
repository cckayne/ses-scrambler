<?php

// Get a ready-to-use one-time pad from Random.org
// <invoke me from the command-line with "php getotp.php">

// size will depend on the anticipated length of your message
// <try not to overload their servers with excessive requests>
$otp_size	= 1000;
$otp_data 	= '';
$otp_url	= 'http://www.random.org/integers/?num='.$otp_size.'&min=0&max=25&col=1&base=10&format=plain&rnd=new';
$otp_fname	= 'otp.txt';

// retrieve the random data
$otp_data	= file_get_contents($otp_url);

// TEST
echo $otp_data;

// save the retrieved OTP data to a local file
// <if I'm called from your SES directory, that's where it'll be put>
// <and that's where you want it to be>
file_put_contents($otp_fname, $otp_data);

?>
