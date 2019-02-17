import { Injectable } from '@angular/core';
import { HttpClient,HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { User } from '../models/user';
import { LoginAttempt } from '../models/login-attempt';
import { Signup } from '../models/signup';
import { Debate } from '../models/debate';
import { Opinion } from '../models/opinion';
import { Vote } from '../models/vote';

@Injectable({
  providedIn: 'root'
})
export class UserService {

	public static readonly LOGIN_URL = "/api/attempt-login";
	public static readonly LOGOUT_URL = "/api/logout";
	public static readonly CREATE_USER_URL = "/api/create-user";
	public static readonly CREATE_DEBATE_URL = "/api/create-debate";
	public static readonly DEBATE_LIST_URL = "/api/debate-list";
	public static readonly DEBATE_URL = "/api/debate/";
	public static readonly OPINION_PAIR_URL = "/api/opinion-pair/";
	public static readonly VOTE_URL = "/api/vote";

	currentUser:User = null;
	private readonly postHttpOptions = {
		headers: new HttpHeaders({
		  'Content-Type':  'application/json',
		  'Authorization': 'my-auth-token'
		})
	  };

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

	signup(signupData:Signup):Observable<User>{
		return this.http.post<User>(UserService.CREATE_USER_URL,signupData,this.postHttpOptions);
	}

	createDebate(debate:Debate):Observable<Debate>{
		return this.http.post<Debate>(UserService.CREATE_USER_URL,debate,this.postHttpOptions);
	}

	getAllDebates():Observable<Debate[]>{
		return this.http.get<Debate[]>(UserService.DEBATE_LIST_URL);
	}

	getDebate(debateId:number):Observable<Debate>{
		return this.http.get<Debate>(UserService.DEBATE_URL+debateId);
	}

	getOpinionPairForVoting(debateId:number):Observable<Opinion[]>{
		return this.http.get<Opinion[]>(UserService.OPINION_PAIR_URL+debateId+"/"+this.currentUser.id);
	}

	vote(vote:Vote):Observable<User>{
		return this.http.post<User>(UserService.VOTE_URL,vote,this.postHttpOptions);
	}
}
