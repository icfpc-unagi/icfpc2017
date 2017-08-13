#!/usr/bin/php
<?php

function Message($color, $message) {
  fwrite(STDERR, "\033[{$color}m" . trim($message) . "\033[0m\n");
}

function Info($message) {
  Message('0;32', $message);
}

function Error($message) {
  Message('0;31', $message);
}

function Read($handle) {
  $length = '';
  while (($c = fgetc($handle)) !== FALSE) {
    if ($c == ':') break;
    $length .= $c;
  }
  if ($length === '') {
    Info('Reaches EOF.');
    return NULL;
  }
  if ($c != ':') {
    Error("':' is expected, but '$c'.");
  }
  if ($length == '-') {
    $message = fgets($handle, 10000);
  } else {
    $length = intval($length);
    $message = '';
    while (TRUE) {
      $remaining_length = $length - strlen($message);
      if ($remaining_length == 0) break;
      if ($remaining_length < 0) {
        Error("Remaining length is negative: " . $remaining_length);
      }
      $buffer = fread($handle, $remaining_length);
      if ($buffer === FALSE) break;
      $message .= $buffer;
    }
    if (strlen($message) != $length) {
      Error("Wrong length: expected $length, but " . strlen($message));
    }
  }
  $result = json_decode($message, TRUE);
  if ($result === NULL) {
    Error("Failed to decode message: '$message'");
  }
  return $result;
}

function Write($handle, $object) {
  $json = json_encode($object) . "\n";
  fwrite($handle, strlen($json) . ':' . $json);
  fflush($handle);
}


function Handshake() {
  global $PROCS;

  // Handshake with children.
  foreach ($PROCS as $index => $proc) {
    $input = Read($proc['pipes'][1]);
    Info("Handshake with child $index: " . json_encode($input));
    Write($proc['pipes'][0], ['you' => $input['me']]);
  }

  // Handshake with parent.
  Write(STDOUT, ['me' => 'Unagi Wrapper']);
  $handshake = Read(STDIN);
  if (!isset($handshake['you'])) {
    Error("Failed to handshake.");
  }
  Info("Handshake with parent: " . json_encode($handshake));
}

function Start() {
  global $argv, $PROCS;

  $PROCS = [];
  for ($i = 1; $i < count($argv); $i++) {
    $proc = [];
    $proc['proc'] = proc_open(
        $argv[$i], [['pipe', 'r'], ['pipe', 'w']], $pipes);
    $proc['pipes'] = $pipes;
    $PROCS[] = $proc;
  }
}

function RunCommand($command) {
  global $PROCS;

  if (isset($command['map'])) {
    $settings = isset($command['settings']) ? $command['settings'] : [];
    if (@$settings['options']) {
      $child = 1;
    } else {
      $child = 0;
    }
    if (isset($command['state'])) {
      unset($command['state']);
    }
  } else {
    $child = $command['state']['child'];
    $command['state'] = $command['state']['state'];
  }

  Write($PROCS[$child]['pipes'][0], $command);
  $result = Read($PROCS[$child]['pipes'][1]);
  $result['state'] = [
      'child' => $child,
      'state' => $result['state']];
  return $result;
}

function ReadCommand() {
  global $PROCS;

  $command = Read(STDIN);

  if (!isset($command['stop'])) {
    Write(STDOUT, RunCommand($command));
  }

  foreach ($PROCS as $proc) {
    fclose($proc['pipes'][0]);
    fclose($proc['pipes'][1]);
    proc_close($proc['proc']);
  }
}

Start();
Handshake();
ReadCommand();
