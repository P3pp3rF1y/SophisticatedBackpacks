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
	private final Set<Integer> errorInventoryParts;

	private UpgradeSlotChangeResult(boolean successful,
			@Nullable ITextComponent errorMessage, Set<Integer> errorUpgradeSlots, Set<Integer> errorInventorySlots, Set<Integer> errorInventoryParts) {
		this.successful = successful;
		this.errorMessage = errorMessage;
		this.errorUpgradeSlots = errorUpgradeSlots;
		this.errorInventorySlots = errorInventorySlots;
		this.errorInventoryParts = errorInventoryParts;
	}

	public Set<Integer> getErrorInventorySlots() {
		return errorInventorySlots;
	}

	public Set<Integer> getErrorInventoryParts() {
		return errorInventoryParts;
	}

	public static class Fail extends UpgradeSlotChangeResult {
		public Fail(ITextComponent errorMessage, Set<Integer> errorUpgradeSlots, Set<Integer> errorInventorySlots, Set<Integer> errorInventoryParts) {
			super(false, errorMessage, errorUpgradeSlots, errorInventorySlots, errorInventoryParts);
		}
	}

	public static class Success extends UpgradeSlotChangeResult {
		public Success() {
			super(true, null, Collections.emptySet(), Collections.emptySet(), Collections.emptySet());
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
