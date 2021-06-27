package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.util.text.ITextComponent;

import javax.annotation.Nullable;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;

public class UpgradeSlotChangeResult {
	private final boolean successful;
	@Nullable
	private final ITextComponent errorMessage;
	private final Set<Integer> errorUpgradeSlots;
	private final Set<Integer> errorInventorySlots;

	private UpgradeSlotChangeResult(boolean successful,
			@Nullable ITextComponent errorMessage, Set<Integer> errorUpgradeSlots, Set<Integer> errorInventorySlots) {
		this.successful = successful;
		this.errorMessage = errorMessage;
		this.errorUpgradeSlots = errorUpgradeSlots;
		this.errorInventorySlots = errorInventorySlots;
	}

	public Set<Integer> getErrorInventorySlots() {
		return errorInventorySlots;
	}

	public static class Fail extends UpgradeSlotChangeResult {
		public Fail(ITextComponent errorMessage, Set<Integer> errorUpgradeSlots, Set<Integer> errorInventorySlots) {
			super(false, errorMessage, errorUpgradeSlots, errorInventorySlots);
		}
	}

	public static class Success extends UpgradeSlotChangeResult {
		public Success() {
			super(true, null, Collections.emptySet(), Collections.emptySet());
		}
	}

	public boolean isSuccessful() {
		return successful;
	}

	public Optional<ITextComponent> getErrorMessage() {
		return Optional.ofNullable(errorMessage);
	}

	public Set<Integer> getErrorUpgradeSlots() {
		return errorUpgradeSlots;
	}
}
