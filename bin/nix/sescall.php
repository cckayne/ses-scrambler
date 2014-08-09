<?php

// How to invoke SES from another program or language:
// <Please ensure that ses(.exe) is somewhere in your system PATH>
// <Failing that, place ses(.exe) in the same folder as the calling program.>
// Example - call SES from PHP and return result of
// 1. Encipherment
// 2. Decipherment

$key = 'england expects';
$msg = 'the quick brown';
$ctx = '';
$ptx = '';

// 1. Encipher
$ctx = exec('ses -e "' . $msg . '" -k "' . $key . '"');
echo $ctx . "\n";
// 2. Decipher
$ptx = exec('ses -d "' . $ctx . '" -k "' . $key . '"');
echo $ptx . "\n";

?>