<?php
	// OTP Authentication Basics
	// (c) Patrick Prémartin
	//
	// Distributed under license AGPL.
	//
	// Infos and updates :
	// https://github.com/DeveloppeurPascal/OTP-Authentication-Basics
	
	session_start();
	require_once(__DIR__."/inc/functions.inc.php");
	require_once(__DIR__."/inc/config.inc.php");

	// This page is only available when no user is connected.
	if (hasCurrentUser()) {
		header("location: index.php");
		exit;
	}

	define("COTPAuthForm", 1);
	define("COTPAuthWait", 2);

	$OTPAuthStatus = COTPAuthForm;

	$error = false;
	$error_message = "";

	if (isset($_POST["frm"]) && ("1" == $_POST["frm"])) {
		$email = isset($_POST["user"])?trim(strip_tags($_POST["user"])):"";
		if (empty($email)) {
			$error = true;
			$error_message .= "Fill your user email address to connect.\n";
		}
		else {
			// TODO : filter the email to avoid spams
			$db = getPDOConnection();
			if (! is_object($db)) {
				$error = true;
				$error_message .= "Database access error. Contact the administrator.\n";
				$OTPAuthStatus = COTPAuthForm;
			}
			else {
				$qry = $db->prepare("select id, enabled from users where email=:email limit 0,1");
				$qry->execute(array(":email" => $email));
				if ((false !== ($rec = $qry->fetch(PDO::FETCH_OBJ))) && (0 == $rec->enabled)) {
					$error = true;
					$error_message .= "Access denied.\n";
					$OTPAuthStatus = COTPAuthForm;
				}
				if (! $error) {
					require_once(__DIR__."/inc/functions-OTP.inc.php");
					OTP_Send_Code($email);
					$OTPAuthStatus = COTPAuthWait;
				}
			}
		}
	}
	else if (isset($_POST["frm"]) && ("2" == $_POST["frm"])) {
		$email = isset($_POST["user"])?trim(strip_tags($_POST["user"])):"";
		if (empty($email)) {
			$error = true;
			$error_message .= "Fill your user email address to connect.\n";
		}
		else {
			// TODO : filter the email to avoid spams
			$OTP_code = isset($_POST["code"])?trim(strip_tags($_POST["code"])):"";
			if (empty($OTP_code)) {
				$error = true;
				$error_message .= "Give the code to connect.\n";
				$OTPAuthStatus = COTPAuthWait;
			}
			else {
				$db = getPDOConnection();
				if (! is_object($db)) {
					$error = true;
					$error_message .= "Database access error. Contact the administrator.\n";
					$OTPAuthStatus = COTPAuthForm;
				}
				else {
					$qry = $db->prepare("select id, enabled from users where email=:email limit 0,1");
					$qry->execute(array(":email" => $email));
					if ((false !== ($rec = $qry->fetch(PDO::FETCH_OBJ))) && (0 == $rec->enabled)) {
						$error = true;
						$error_message .= "Access denied.\n";
						$OTPAuthStatus = COTPAuthForm;
					}
					if (! $error) {
						require_once(__DIR__."/inc/functions-OTP.inc.php");
						switch (OTP_Check_Code($email, $OTP_code)) {
							case OTPCheckOK :
								if (! is_object($rec)) {
									$qry = $db->prepare("insert into users (email, enabled, create_ip, create_datetime) values (:e,1,:ci,:cdt)");
									if (! $qry->execute(array(":e" => $email, ":ci" => $_SERVER["REMOTE_ADDR"], ":cdt" => date("YmdHis")))) {
										$error = true;
										$error_message .= "Can't add this user in the database.\n";
										$OTPAuthStatus = COTPAuthForm;
									}
									else {
										$id = $db->lastInsertId();
									}
								}
								else {
									$id = $rec->id;
								}
								if (! $error) {
									$qry = $db->prepare("update users set email_checked=1, email_check_ip=:ip, email_check_datetime=:dt where id=:id");
									$qry->execute(array(":ip" => $_SERVER["REMOTE_ADDR"], ":dt" => date("YmdHis"), ":id" => $id));
									setCurrentUserId($id);
									setCurrentUserEmail($email);
									header("location: ".URL_CONNECTED_USER_HOMEPAGE);
									exit;
								}
								break;
							case OTPCheckExpired:
								$error = true;
								$error_message .= "Too late. The code expired. Send a new one.\n";
								$OTPAuthStatus = COTPAuthForm;
								break;
							case OTPCheckWrong:
								$error = true;
								$error_message .= "Wrong code, please retry or resend one.\n";
								$OTPAuthStatus = COTPAuthWait;
								break;
							case OTPCheckUnknow:
								$error = true;
								$error_message .= "LOL\n";
								$OTPAuthStatus = COTPAuthForm;
								break;
						}
					}
				}
			}
		}
	}
?><!DOCTYPE html>
<html lang="en">
	<head>
		<meta charset="UTF-8">
		<meta http-equiv="content-type" content="text/html; charset=UTF-8">
		<meta name="viewport" content="width=device-width, initial-scale=1, user-scalable=yes">
		<title>Log or sign in - OTP Authentication Basics</title>
		<style>
			.error {
				color: red;
				background-color: yellow;
			}
		</style>
	</head>
	<body><?php include_once(__DIR__."/inc/header.inc.php"); ?>
		<h2>Log or sign in</h2><?php
	if ($error && (! empty($error_message))) {
		print("<p class=\"error\">".nl2br($error_message)."</p>");
	}

	switch ($OTPAuthStatus) {
		case COTPAuthForm:
?><form method="POST" action="login.php" onSubmit="return ValidForm();"><input type="hidden" name="frm" value="1">
			<p>
				<label for="User">User email</label><br>
				<input id="User" name="user" type="email" value="<?php print(isset($email)?htmlspecialchars($email):""); ?>" prompt="Your email address">
			</p>
			<p>
				<button type="submit">Connect</button>
			</p>
		</form>
<script>
	document.getElementById('User').focus();
	function ValidForm() {
		email = document.getElementById('User');
		if (0 == email.value.length) {
			email.focus();
			window.alert('Your email address is needed !');
			return false;
		}
		return true;
	}
</script><?php
			break;
		case COTPAuthWait:
?><p>We sent a code by email to your address. Please write it below.</p>
<form id="MyForm" method="POST" action="login.php" onSubmit="return ValidForm();"><input type="hidden" id="Frm" name="frm" value="2"><input id="User" name="user" type="hidden" value="<?php print(isset($email)?htmlspecialchars($email):""); ?>">
	<p>
		<label for="Code">Received code</label><br>
		<input id="Code" name="code" type="text" value="" prompt="The temporary code we sent you">
	</p>
	<p>
		<button type="submit">Connect</button>
	</p>
</form>
<p>Of course check your spams if you didn't see it in your inbox or you can <a href="#" onclick="ReSendCode();">ask a new code</a>.</p><script>
	document.getElementById('Code').focus();
	function ValidForm() {
		code = document.getElementById('Code');
		if (0 == code.value.length) {
			code.focus();
			window.alert('The code is needed !');
			return false;
		}
		else if (6 != code.value.length) {
			code.focus();
			window.alert('Wrong code !');
			return false;
		}
		// TODO : check if the code has only 6 numbers (or other scheme depending on how its generated)
		return true;
	}
	function ReSendCode() {
		document.getElementById('Frm').value = '1';
		document.getElementById('Code').value = 'XXXXXXXXXX';
		document.getElementById('MyForm').submit();
	}
</script><?php
			break;
		default :
	}
	include_once(__DIR__."/inc/footer.inc.php"); ?></body>
</html>