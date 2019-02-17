import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { VoteDistributionComponent } from './vote-distribution.component';

describe('VoteDistributionComponent', () => {
  let component: VoteDistributionComponent;
  let fixture: ComponentFixture<VoteDistributionComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ VoteDistributionComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(VoteDistributionComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
