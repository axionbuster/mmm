# Login

## Clientbound

### Disconnect (login)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Reason               | String   | Disconnect message   | 0x00         |
+----------------------+----------+----------------------+--------------+

### Encryption Request
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Server ID            | String   | Unique server ID     | 0x01         |
| Public Key           | String   | RSA public key       |              |
| Verify Token         | String   | Token for verification|              |
+----------------------+----------+----------------------+--------------+

### Login Success
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| UUID                 | String   | Player UUID          | 0x02         |
| Username             | String   | Player name          |              |
+----------------------+----------+----------------------+--------------+

### Set Compression
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Threshold            | VarInt   | Compression threshold| 0x03         |
+----------------------+----------+----------------------+--------------+

### Login Plugin Request
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Channel              | String   | Plugin channel name  | 0x04         |
| Data                 | Byte[]   | Plugin data          |              |
+----------------------+----------+----------------------+--------------+

### Cookie Request (login)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Cookie               | String   | Authentication cookie| 0x05         |
+----------------------+----------+----------------------+--------------+

## Serverbound

### Login Start
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Username             | String   | Player username      | 0x00         |
+----------------------+----------+----------------------+--------------+

### Encryption Response
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Shared Secret        | Byte[]   | Encrypted shared secret| 0x01       |
| Verify Token         | Byte[]   | Encrypted verify token|              |
+----------------------+----------+----------------------+--------------+

### Login Plugin Response
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Channel              | String   | Plugin channel name  | 0x02         |
| Data                 | Byte[]   | Plugin response data |              |
+----------------------+----------+----------------------+--------------+

### Login Acknowledged
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| -                    | -        | Acknowledgment       | 0x03         |
+----------------------+----------+----------------------+--------------+

### Cookie Response (login)
+----------------------+----------+----------------------+--------------+
| Field Name           | Type     | Notes                | Protocol ID  |
+----------------------+----------+----------------------+--------------+
| Cookie               | String   | Authentication cookie| 0x04         |
+----------------------+----------+----------------------+--------------+
