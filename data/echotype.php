#!/usr/bin/php
<?php
/**
** Delay-output text from stdin, crudely like a human/mechanical typist.
*/

define('LF', chr(10));
define('BK', chr(8));
$m = isset($argv[1])? $argv[1]: 1;
$L = shell_exec('tput cols')-1;
$w = ''; $l = 0;
while (!feof(STDIN)) {
	$c = fread(STDIN,1);
	$p = 0;
	switch ($c) {
	case LF:
		$l = -1;
	case ' ':
		$w = '';
		$p += 75000;
		if ($l == $L) { $c = LF; $l = -1; }
		break;
	case '.': case '?': case '!':
		$p += 150000;
	case ',': case '-':
		$p += 150000;
	default:
		$p += 75000;
		if ($l == $L) {
			if (($W = strlen($w)) < $L) {
				echo str_repeat(BK, $W).str_repeat(' ',$W).LF.$w; $l = $W;
			} else {
				echo LF; $w=''; $l = 0;
			}
		}
		$w .= $c;
		break;
	}
	echo $c;
	usleep($p / $m);
	$l++;
}

?>
