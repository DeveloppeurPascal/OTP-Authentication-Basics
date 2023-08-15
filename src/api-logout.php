<?php
	// OTP Authentication Basics
	// (c) Patrick Prémartin
	//
	// Distributed under license AGPL.
	//
	// Infos and updates :
	// https://github.com/DeveloppeurPascal/OTP-Authentication-Basics

	// Close a session (or disconnect)
	//
	// IN : (POST request)
	//		sessid : ID de session à clôturer
	//		k : code de vérification (signature de la requête par rapport à d'autres infos connues seulement de l'application et du site) - TODO

	// OUT as stringified JSON object : (ok - status code 200)
	//		error : false

	// OUT as stringified JSON object : (error - status code 403)
	//		error : true
	//		errortext : comment as string

	header('Content-Type: application/json; charset=utf8');
	header('Access-Control-Allow-Origin: *'); // better to fix the domain or allowed domains

	$result = new stdClass();
	$result->error = false;

	// TODO : manage the Application ID if available
	// TODO : manage the Device ID if available

	$SessionID = isset($_POST["sessid"])?trim(strip_tags($_POST["sessid"])):"";
	if (empty($SessionID)) {
		$result->error = true;
		$result->errortext = "Hello World !";
	}
	else {
		require_once(__DIR__."/inc/functions-temp.inc.php");
		$TempData = LoadTempFile(getTempFileName($SessionID));
		if (isset($TempData->Sessions[$SessionID])) {
			unset($TempData->Sessions[$SessionID]);
			SaveTempFile($TempData);
		}
		else {
			$result->error = true;
			$result->errortext = "Hu hu hu";
		}
	}

	if ($result->error) {
		http_response_code(403);
	}
	else {
		http_response_code(200);
	}
	print(json_encode($result));
