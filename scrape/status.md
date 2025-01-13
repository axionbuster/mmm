# Status

## Clientbound

### Status Response (JSON)
+-----------------+----------+------------------------+--------------+
| Field Name      | Type     | Notes                  | Protocol ID  |
+-----------------+----------+------------------------+--------------+
| Version         | String   | Minecraft version      | 0x00         |
| Players         | Object   | Player information     |              |
| Description     | String   | Server description     |              |
| Favicon         | String   | Base64 encoded image   |              |
+-----------------+----------+------------------------+--------------+

### Pong Response (status)
+-------------+-------+-----------------------+--------------+
| Field Name  | Type  | Notes                 | Protocol ID  |
+-------------+-------+-----------------------+--------------+
| Payload     | Long  | Echo of server's ping | 0x01         |
+-------------+-------+-----------------------+--------------+

## Serverbound

### Status Request
+-----------+------+-----------+--------------+
| Field Name| Type | Notes     | Protocol ID  |
+-----------+------+-----------+--------------+
| Request   | -    | -         | 0x00         |
+-----------+------+-----------+--------------+

### Ping Request (status)
+-----------+-------+-------------+--------------+
| Field Name| Type  | Notes       | Protocol ID  |
+-----------+-------+-------------+--------------+
| Payload   | Long  | Timestamp   | 0x01         |
+-----------+-------+-------------+--------------+
