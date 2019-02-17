import { Injectable } from '@angular/core';
import { HttpClient,HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { User } from '../models/user';
import { LoginAttempt } from '../models/login-attempt';

@Injectable({
  providedIn: 'root'
})
export class UserService {

	public static readonly LOGIN_URL = "/api/attempt-login";
	public static readonly LOGOUT_URL = "/api/logout";

	currentUser:User = null;

	constructor(private http:HttpClient) { }

	testSimpleEndpoint():Observable<any>{
		return this.http.get<any>("/api/subject/23");
	}

	attemptLogin(loginAttempt:LoginAttempt):Observable<User>{
		const httpOptions = {
			headers: new HttpHeaders({
			  'Content-Type':  'application/json',
			  'Authorization': 'my-auth-token'
			})
		  };
		return this.http.post<User>(UserService.LOGIN_URL,loginAttempt,httpOptions);
	}

	logout():Observable<boolean>{
		const httpOptions = {
			headers: new HttpHeaders({
			  'Content-Type':  'application/json',
			  'Authorization': 'my-auth-token'
			})
		  };
		return this.http.post<boolean>(UserService.LOGOUT_URL,this.currentUser,httpOptions);
	}
}
