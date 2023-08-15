<?php
	// OTP Authentication Basics
	// (c) Patrick Prémartin
	//
	// Distributed under license AGPL.
	//
	// Infos and updates :
	// https://github.com/DeveloppeurPascal/OTP-Authentication-Basics

	// Check an OTP code
	//
	// IN : (POST request)
	//		token de demande (application autorisée sur un device autorisé) - TODO
	// 		email : mail de l'utilisateur
	// 		code : code saisi par l'utilisateur, à comparer avec l'OTP

	// OUT as stringified JSON object : (ok - status code 200)
	//		error : false
	//		sessid : ID de session

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
		$code = isset($_POST["code"])?trim(strip_tags($_POST["code"])):"";
		if (empty($code)) {
			$result->error = true;
			$result->errortext = "Empty code.";
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
					switch (OTP_Check_Code($email, $code)) {
						case OTPCheckOK :
							if (! is_object($rec)) {
								$qry = $db->prepare("insert into users (email, enabled, create_ip, create_datetime) values (:e,1,:ci,:cdt)");
								if (! $qry->execute(array(":e" => $email, ":ci" => $_SERVER["REMOTE_ADDR"], ":cdt" => date("YmdHis")))) {
									$result->error = true;
									$result->errortext = "Can't add this user in the database.";
								}
								else {
									$id = $db->lastInsertId();
								}
							}
							else {
								$id = $rec->id;
							}
							if (! $result->error) {
								$qry = $db->prepare("update users set email_checked=1, email_check_ip=:ip, email_check_datetime=:dt where id=:id");
								$qry->execute(array(":ip" => $_SERVER["REMOTE_ADDR"], ":dt" => date("YmdHis"), ":id" => $id));
								require_once(__DIR__."/inc/functions-temp.inc.php");
								$NbCar = 25;
								do {
									$SessionID = getNewIdString($NbCar++);
									$TempData = LoadTempFile(getTempFileName($SessionID));
									if (! isset($TempData->Sessions)) {
										$TempData->Sessions = array();
									}
								}
								while (isset($TempData->Sessions[$SessionID]));
								$TempData->Sessions[$SessionID] = new stdClass();
								// $TempData->Sessions[$SessionID]->AppID = $id; 
								// $TempData->Sessions[$SessionID]->DeviceID = $id; 
								$TempData->Sessions[$SessionID]->id = $id;
								$TempData->Sessions[$SessionID]->email = $email;
								$TempData->Sessions[$SessionID]->datetime = time();
								$result->sessid = $SessionID;
								// TODO : manage sessions a an include file with global functions (getSessionValue / setSessionValue)
							}
							break;
						case OTPCheckExpired:
							$result->error = true;
							$result->errortext = "Too late, code expired.";
							break;
						case OTPCheckWrong:
							$result->error = true;
							$result->errortext = "Access denied.";
							break;
						case OTPCheckUnknow:
							$result->error = true;
							$result->errortext = "Access denied.";
							break;
						default :
							die("unknow response from OTP check");
					}
				}
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
