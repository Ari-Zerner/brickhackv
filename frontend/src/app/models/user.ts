export class User {
	id:number;
	username:string;
	name:string;
	email:string;
	// IMPORTANT we dont hold password on client side
	authenticated:boolean;
}
