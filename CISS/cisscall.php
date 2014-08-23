<?php

// How to invoke CISS from another program or language
// ---------------------------------------------------
// <Please ensure that ciss(.exe) is somewhere in your system PATH>
// <Failing that, place ciss(.exe) in the same folder as the calling program.>
// Example - call CISS from PHP and return result of
// 1. Encipherment
// 2. Decipherment

// declare main constants & variables
$key = 'england expects every man to do his duty';
$msg = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz1234567890,.+*{}[]%&/()=?\<>!#@-_;:'."'";
$ctx = '';
$ptx = '';
$wct = str_word_count($key);
echo "\n";


// timer start
$time_pre = microtime(true);

// 1. CISS Encipher
$ctx = exec('ciss "' . $msg . '" "' . $key . '"' . ' e m a');
// 2. SES Decipher
$ptx = exec('ciss "' . $ctx . '" "' . $key . '"' . ' d m a');

// timer stop
$time_post = microtime(true);
$exec_time = $time_post - $time_pre;

// show ciphertext output
echo $ctx . "\n";
// show decrypted plaintext
echo "\n". $ptx . "\n";

// report timings
echo "\n >> CISS encipher & decipher call took " . $exec_time . ' sec <<';
echo "\n";
?>