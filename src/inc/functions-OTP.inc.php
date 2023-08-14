<?php
	// OTP Authentication Basics
	// (c) Patrick Prémartin
	//
	// Distributed under license AGPL.
	//
	// Infos and updates :
	// https://github.com/DeveloppeurPascal/OTP-Authentication-Basics

function OTP_Send_Code($email) {
	$OTP_code = getNewIdNumber(6);
	require_once(__DIR__."/functions-temp.inc.php");
	$TempData = LoadTempFile(getTempFileName($email));
	if (! isset($TempData->OTPCode)) {
		$TempData->OTPCode = array();
	}
	if (! isset($TempData->OTPCode[$email])) {
		$TempData->OTPCode[$email] = new stdClass();
	}
	$TempData->OTPCode[$email]->CodeOTP = $OTP_code;
	$TempData->OTPCode[$email]->Expiration = time()+60*60; // 1 hour
	SaveTempFile($TempData);
	if (_DEBUG)
	{
		mail($email, "Connection code", "Hi\n\nHere is your new code for site XXX : ".$OTP_code."\n\nBest regards\n\nThe team");
	}
	else {
		// TODO : replace this by an email check link
		die("Sending a connexion email is not available here.");
	}
}

define("OTPCheckOK",0);
define("OTPCheckWrong",1);
define("OTPCheckExpired",2);
define("OTPCheckUnknow",3);
function OTP_Check_Code($email, $OTP_code) {
	require_once(__DIR__."/functions-temp.inc.php");
	$TempData = LoadTempFile(getTempFileName($email));
	if (isset($TempData->OTPCode[$email])) {
		$code = isset($TempData->OTPCode[$email]->CodeOTP)?$TempData->OTPCode[$email]->CodeOTP:false;
		$expiration = isset($TempData->OTPCode[$email]->Expiration)?$TempData->OTPCode[$email]->Expiration:0;
		if ($code == $OTP_code) {
			if ($expiration >= time()) {
				unset($TempData->OTPCode[$email]);
				SaveTempFile($TempData);
				return OTPCheckOK;
			}
			else {
				return OTPCheckExpired;
			}
		}
		else {
			return OTPCheckWrong;
		}
	}
	else {
		return OTPCheckUnknow;
	}
}
