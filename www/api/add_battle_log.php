<?php

require_once(dirname(__FILE__) . '/../library/api.php');

if (!Database::SelectCell(
        'SELECT battle_id FROM battle WHERE battle_id = {battle_id}',
        ['battle_id' => intval($_GET['battle_id'])])) {
  die(json_encode(['error' => 'Unknown battle_id: ' . $_GET['battle_id']]));
}

if (!isset($_POST['battle_log_data'])) {
  die(json_encode(['error' => 'battle_log_data must be given by POST.']));
}

Database::Command('INSERT INTO battle_log SET battle_id = {battle_id}, battle_log_data = {battle_log_data}', ['battle_id' => intval($_GET['battle_id']), 'battle_log_data' => $_POST['battle_log_data']]);
die(json_encode(['affected_rows' => Database::AffectedRows()]));

