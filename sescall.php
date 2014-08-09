<?php

// How to invoke SES from another program or language
// --------------------------------------------------
// <Please ensure that ses(.exe) is somewhere in your system PATH>
// <Failing that, place ses(.exe) in the same folder as the calling program.>
// Example - call SES from PHP and return result of
// 1. Encipherment
// 2. Decipherment

// declare main constants & variables
$key = 'england expects every man to do his duty';
$msg = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz1234567890,.+*{}[]%&/()=?\<>!#@-_;:'."'";
$ctx = '';
$ptx = '';
$wct = str_word_count($key);
echo "\n";

// report keyword count
echo 'Calling SES with ' . $wct . '-word key (' . ($wct+1) . " super-encypherments)...\n\n";

// timer start
$time_pre = microtime(true);

// 1. SES Encipher
$ctx = exec('ses -e "' . $msg . '" -k "' . $key . '"');
// 2. SES Decipher
$ptx = exec('ses -d "' . $ctx . '" -k "' . $key . '"');

// timer stop
$time_post = microtime(true);
$exec_time = $time_post - $time_pre;

// show ciphertext output
echo $ctx . "\n";
// show decrypted plaintext
echo "\n". $ptx . "\n";

// report timings
echo "\n >> SES encipher & decipher call took " . $exec_time . ' sec <<';
echo "\n   ( in cryptography, slow is GOOD :) )\n";

?>