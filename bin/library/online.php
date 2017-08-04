<?php

function Message($color, $message) {
  fwrite(STDERR, "\033[{$color}m" . trim($message) . "\033[0m\n");
}

function Fatal($message) {
  Message('0;31', $message);
  exit(1);
}

function Error($message) {
  Message('0;31', $message);
}

$host = getenv('PUNTER_HOST') ?: 'punter.inf.ed.ac.uk';
$port = getenv('PUNTER_PORT') ?: Fatal('PUNTER_PORT must be specified.');
$fp = fsockopen($host, $port);
$punter = getenv('PUNTER') ?: Fatal('PUNTER must be specified,');

function Command($command) {
  fwrite(STDOUT, rtrim($command, "\r\n") . "\n");
  $response = rtrim(fgets(STDIN, 10000000), "\r\n");
  $result = explode(' ', $response, 2);
  if ($result[0] != 'OK') {
    Fatal("Ninestream failure: $response: $command");
  }
  if (count($result) == 2) {
    return $result[1];
  }
  return TRUE;
}

function ReadFromServer() {
  global $fp;
  $bytes = '';
  while (TRUE) {
    $digit = fread($fp, 1);
    if ($digit == ':') break;
    if (!ctype_digit($digit)) {
      Fatal("Invalid server response: '$bytes$digit'");
    }
    $bytes .= $digit;
  }
  $result = fread($fp, intval($bytes));
  Message('0;32', rtrim("Read from server: $result") . "\n");
  $object = json_decode($result, TRUE);
  if (!is_array($object)) {
    Fatal("Invalid JSON from stream $stream_id: $result");
  }
  return $object;
}

function WriteToServer($object) {
  global $fp;
  $json = json_encode($object) . "\n";
  fwrite($fp, strlen($json)  . ":$json");
  fflush($fp);
  Message('0;32', rtrim("Write to server: $json") . "\n");
}

function RunAi($command) {
  global $punter;
  Message('0;34', "Write to AI: " . json_encode($command) . "\n");
  if ($punter != '-') {
    $proc = proc_open($punter, [['pipe', 'r'], ['pipe', 'w']], $pipes);
    $json = json_encode($command) . "\n";
    fwrite($pipes[0], strlen($json) . ":$json");
    fflush($pipes[0]);
    fclose($pipes[0]);
    $input = fgets($pipes[1], 1000000);
    fclose($pipes[1]);
    proc_close($proc);
  } else {
    $input = fgets(STDIN, 1000000);
  }
  $result = explode(':', $input, 2);
  if (count($result) != 2) {
    Fatal("AI must return a line formatted as 'bytes:json', but: '$input'");
  }
  Message('0;34', "Read from AI: " . rtrim($result[1]) . "\n");
  if ($result[0] != '-' && intval($result[0]) == strlen($result[1])) {
    Error('Invalid JSON length from AI: ' . strlen($result[1]) .
          ' is expected, but ' . $result[0]);
  }
  $object = json_decode($result[1], TRUE);
  return $object;
}

function Main() {
  $name = getenv('PUNTER_NAME') ?: 'U Punter';

  WriteToServer(['me' => $name]);
  $you = ReadFromServer();
  if ($you['you'] !== $name) {
    Fatal("Server should echo the punter name '$name', but: " .
          json_encode($you));
  }

  $setup = ReadFromServer();
  $result = RunAi($setup);

  if (!isset($result['state'])) {
    Error('AI does not return state in setting up.');
  } else {
    $state = $result['state'];
    unset($result['state']);
  }
  WriteToServer($result);

  while (TRUE) {
    $operation = ReadFromServer();
    if (isset($operation['move'])) {
      if (isset($state)) {
        $operation['state'] = $state;
      }
      $result = RunAi($operation);
      if (!isset($result['state'])) {
        Error('AI does not return state in move: ' . json_encode($result));
      } else {
        $state = $result['state'];
        unset($result['state']);
      }
      WriteToServer($result);
    } else if (isset($operation['stop'])) {
      fwrite(STDERR, "Result: " . json_encode($operation) . "\n");
      break;
    } else {
      Fatal('Unknown operation from server: ' . json_encode($operation));
    }
  }
}

Main();
