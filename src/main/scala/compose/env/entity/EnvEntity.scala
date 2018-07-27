package compose.env.entity

import java.sql.Timestamp

object EnvEntity {

  case class TSet(id: Int, name: String, env: String, createdAt: Timestamp, updatedAt: Timestamp, remark: String)

  case class THost(id: Int, name: String, setId: Int, ip: Int, labels: String, extra: String, env: String, createdAt: Timestamp, updatedAt: Timestamp, remark: String)

  case class TService(id: Int, name: String, labels: String,env: String, volumns: String, ports: String, composeLabels: String, dockerExtras: String, createdAt: Timestamp, updatedAt: Timestamp, remark: String )

}
