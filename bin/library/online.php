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
Message('0;32', "Connecting to server $host:$port...");
$fp = fsockopen($host, $port);
if (!$fp) {
  Fatal("Failed to connect $host:$port.");
}
Message('0;32', "Connected.");
$punter = getenv('PUNTER') ?: Fatal('PUNTER must be specified,');
$handshake = boolval(getenv('PUNTER_HANDSHAKE'));
$newline = boolval(getenv('PUNTER_NEWLINE'));

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
  $result = '';
  while (intval($bytes) - strlen($result) > 0 &&
         ($buffer = fread($fp, intval($bytes) - strlen($result))) !== FALSE) {
    $result .= $buffer;
  }
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
  global $punter, $handshake, $newline;
  if ($punter != '-') {
    $start_time = microtime(TRUE);
    $proc = proc_open($punter, [['pipe', 'r'], ['pipe', 'w']], $pipes);
    if ($handshake) {
      $input = fgets($pipes[1], 10000);
      $result = explode(':', $input, 2);
      if (count($result) != 2) {
        Fatal("AI must output a handshake formatted as 'bytes:json', " .
              "but: '$input'");
      }
      $me = json_decode($result[1], TRUE);
      if (!isset($me['me'])) {
        Fatal('Handshake must contain me field: ' . $result);
      }
      Message('0;32', "Handshake is done. AI name is '{$me['me']}'.");
      $json = json_encode(['you' => $me['me']]) . ($newline ? "\n" : '');
      Message('0;34', "Write to AI: " . strlen($json) . ":$json");
      fwrite($pipes[0], strlen($json) . ":$json");
    }
    $json = json_encode($command) . ($newline ? "\n" : '');
    Message('0;34', "Write to AI: " . strlen($json) . ":$json");
    fwrite($pipes[0], strlen($json) . ":$json");
    fflush($pipes[0]);
    fclose($pipes[0]);
    $input = fgets($pipes[1], 1000000);
    fclose($pipes[1]);
    proc_close($proc);
    Message('0;34', 'Elapsed time: ' .
                    round((microtime(TRUE) - $start_time) * 1000) . ' ms');
  } else {
    $input = '';
    while (($buffer = fgets(STDIN, 1000)) !== FALSE) {
      $input .= $buffer;
      if (substr($input, -1) == "\n") break;
    }
  }
  $result = explode(':', $input, 2);
  if (count($result) != 2) {
    Fatal("AI must return a line formatted as 'bytes:json', but: '$input'");
  }
  Message('0;34', "Read from AI: " . rtrim($result[1]) . "\n");
  if ($result[0] != '-' && intval($result[0]) != strlen($result[1])) {
    Error('Invalid JSON length from AI: ' . strlen($result[1]) .
          ' is expected, but ' . $result[0]);
  }
  $object = json_decode($result[1], TRUE);
  return $object;
}

function GetRiverId($source, $target) {
  if ($source > $target) return GetRiverId($target, $source);
  return "$source-$target";
}

function Main() {
  $name = getenv('PUNTER_NAME') ?: 'U Punter';

  Message('0;32', "Handshaking with server...");
  WriteToServer(['me' => $name]);
  $you = ReadFromServer();
  if ($you['you'] !== $name) {
    Fatal("Server should echo the punter name '$name', but: " .
          json_encode($you));
  }

  Message('0;32', "Reading setup from server...");
  $setup = ReadFromServer();
  if (!isset($setup['punter'])) {
    Fatal('setup response must have punter field.');
  }
  $punter_id = $setup['punter'];
  Message('1;33', "Your punter: $punter_id");
  if (!isset($setup['map']['rivers'])) {
    Fatal('setup response must have rivers field.');
  }
  $rivers = [];
  foreach ($setup['map']['rivers'] as $river) {
    $rivers[GetRiverId($river['source'], $river['target'])] = -1;
  }

  $result = RunAi($setup);
  if (!isset($result['ready'])) {
    Fatal("AI should return ready.");
  }
  if ($result['ready'] !== $punter_id) {
    Fatal('ready must correspond to punter_id: ' .
          "$punter_id vs {$result['ready']}.");
  }

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
      foreach ($operation['move']['moves'] as $move) {
        if (isset($move['claim'])) {
          $claim = $move['claim'];
          $rivers[GetRiverId($claim['source'], $claim['target'])] =
              $claim['punter'];
        }
      }
      $num_rivers = count($rivers);
      $punter_counts = array_count_values($rivers);
      $num_empty_rivers = $punter_counts[-1] ?: 0;
      $num_my_rivers = @intval($punter_counts[$punter_id]);
      Message('0;33', 'Occupied rivers: ' . ($num_rivers - $num_empty_rivers) .
                      ' (' . $num_my_rivers . ') / ' . $num_rivers);
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
      if (!isset($result['claim']['punter']) ||
          $result['claim']['punter'] != $punter_id) {
        Error("claim.punter must be set to $punter_id: " .
              json_encode($result));
      }
      $river_id = GetRiverId($result['claim']['source'],
                             $result['claim']['target']);
      if (!isset($rivers[$river_id])) {
        Error("AI claims a non-existing river: $river_id");
      }
      if ($rivers[$river_id] != -1) {
        Error('AI claims an occupied river: ' .
              "$river_id is occupied by {$rivers[$river_id]}");
      }
      WriteToServer($result);
    } else if (isset($operation['stop'])) {
      Message('', '===== Result =====');
      foreach ($operation['stop']['scores'] as $score) {
        $is_mine = ($score['punter'] == $punter_id);
        Message($is_mine ? '1;33' : '',
                "Punter {$score['punter']}: {$score['score']}" .
                ($is_mine ? '  <== YOU!' : ''));
      }
      break;
    } else {
      Fatal('Unknown operation from server: ' . json_encode($operation));
    }
  }
}

Main();
