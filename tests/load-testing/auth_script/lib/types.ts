export interface AuthResponse {
  jwt: string;
  user: User;
  stakeAddress?: string;
}

interface User {
  id: number;
  username: string;
  email: string;
  provider: string;
  confirmed: boolean;
  blocked: boolean;
  govtool_username: string | null;
  createdAt: string;
  updatedAt: string;
}
