<?php
	// OTP Authentication Basics
	// (c) Patrick Prémartin
	//
	// Distributed under license AGPL.
	//
	// Infos and updates :
	// https://github.com/DeveloppeurPascal/OTP-Authentication-Basics

	// Send an OTP code by email to the user

	// IN : (POST request)
	//		ID du device demandeur (recommandé si vrai logociel/application mobile) - TODO
	//		ID de l'application demandeuse (recommandé, clé API ou autre) - TODO
	// 		email : mail de l'utilisateur

	// OUT as stringified JSON object : (ok - status code 200)
	//		error : false
	//		token : token de demande - TODO

	// OUT as stringified JSON object : (error - status code 403)
	//		error : true
	//		errortext : comment as string

	header('Content-Type: application/json; charset=utf8');
	header('Access-Control-Allow-Origin: *'); // better to fix the domain or allowed domains

	$result = new stdClass();
	$result->error = false;

	// TODO : manage the Application ID if available
	// TODO : manage the Device ID if available

	$email = isset($_POST["email"])?trim(strip_tags($_POST["email"])):"";
	if (empty($email)) {
		$result->error = true;
		$result->errortext = "Empty email address.";
	}
	else {
		// TODO : filter the email to avoid spams
		
		require_once(__DIR__."/inc/functions.inc.php");
		require_once(__DIR__."/inc/config.inc.php");
		$db = getPDOConnection();
		if (! is_object($db)) {
			$result->error = true;
			$result->errortext = "DB Error.";
		}
		else {
			$qry = $db->prepare("select id, enabled from users where email=:email limit 0,1");
			$qry->execute(array(":email" => $email));
			if ((false !== ($rec = $qry->fetch(PDO::FETCH_OBJ))) && (0 == $rec->enabled)) {
				$result->error = true;
				$result->errortext = "Access denied.";
			}
			if (! $result->error) {
				require_once(__DIR__."/inc/functions-OTP.inc.php");
				OTP_Send_Code($email);
				// TODO : add a token and reply with it
			}
		}
	}

	if ($result->error) {
		http_response_code(403);
	}
	else {
		http_response_code(200);
	}
	print(json_encode($result));
